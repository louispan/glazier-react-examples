{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Todo.Todo where
    -- ( Todo(..)
    -- , _value
    -- , _completed
    -- , _editing
    -- , TodoDestroy
    -- , TodoToggleComplete
    -- , todo
    -- ) where

-- import qualified Data.Aeson as A
-- import qualified Data.Aeson.Applicative as A

import qualified Data.JSString as J
import Data.Tagged.Extras
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Widgets.Input
import qualified JS.DOM as DOM

default (JSString)

data Editing = NotEditing | Focusing | Editing
    deriving (Show, Read, Eq, Ord, G.Generic)

data Todo = Todo
    { value :: JSString
    , completed :: Bool
    , editing :: Editing
    } deriving (Show, Read, Eq, Ord, G.Generic)

makeLenses_ ''Todo

-- instance A.ToJSON Todo where toEncoding = A.genericToEncoding A.defaultOptions
-- instance Applicative m => A.AToJSON m Todo
-- instance A.FromJSON Todo
-- instance Applicative m => A.AFromJSON m Todo

-- type OnTodoDestroy = Tagged "TodoDestroy"
-- type OnTodoToggleComplete = Tagged "TodoToggleComplete"
-- type OnTodoStartEdit = Tagged "TodoStartEdit"
-- data TodoDestroy = TodoDestroy
-- data TodoToggleComplete = TodoToggleComplete
-- data TodoStartEdit = TodoStartEdit

todoToggleComplete ::
    (MonadWidget s m)
    => Traversal' s Todo -> m ()
todoToggleComplete this = checkbox (this._completed)
    [("onChange", onChange)] [("className", "toggle")]
  where
    onChange = mkHandler' (const $ pure ()) (const $ noisyMutate $ this._completed %= not)

todoDestroy :: (MonadWidget s m, MonadObserver' (Tagged "TodoDestroy" ()) m) => m ()
todoDestroy = lf "button" [("onClick", onClick)]
    [("key", "destroy"),("className", "destroy")]
  where
    onClick = mkHandler' (const $ pure ()) $
        const $ observe' $ Tagged @"TodoDestroy" ()

todoLabel :: (MonadWidget s m, MonadObserver' (Tagged "TodoStartEdit" ()) m)
    => Traversal' s Todo -> m ()
todoLabel this = bh "label" [("onDoubleClick", onDoubleClick)] []
    (txt (model $ this._value))
  where
    onDoubleClick = mkHandler' (const $ pure ()) $ const $ observe' $ Tagged @"TodoStartEdit" ()

todoInput ::
    ( MonadWidget s m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    )
    => Traversal' s Todo
    -> m ()
todoInput this = input (this._value)
    [("onBlur", onBlur), ("onKeyDown", onKeyDown)]
    [("className", "edit")]
  where
    onBlur = mkHandler' (const $ pure ()) (const $
        noisyMutate $ do
            this._editing .= NotEditing
            this._value %= J.strip)

    onKeyDown = mkHandler' fromKeyDown hdlKeyDown
    fromKeyDown evt = do
        t <- viaJS @DOM.HTMLElement <$> DOM.target evt
        guardJustM $ pure $ (\t' e' -> (t', DOM.key e')) <$> t <*> e
      where
        e = viaJS @SyntheticKeyboardEvent evt
    hdlKeyDown (t, k) = case k of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with input onChange
            -- updating the value under our feet.
            "Enter" -> do
                    a <- guardJustM $ noisyMutate $ do
                        v <- use (this._value)
                        let v' = J.strip v
                        if J.null v'
                            then
                                pure $ Just $ Tagged @"TodoDestroy" ()
                            else do
                                this._editing .= NotEditing
                                this._value .= v'
                                pure Nothing
                    observe' a

            "Escape" -> DOM.blur t -- The onBlur handler will trim the value

            _ -> pure ()

todoView ::
    ( MonadWidget s m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    , MonadObserver' (Tagged "TodoStartEdit" ()) m
    )
    => Traversal' s Todo
    -> m ()
todoView this = do
    bh "div" [] [("className", "view")] $ todoToggleComplete this
    todoLabel this
    todoDestroy

todo ::
    ( MonadWidget s m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    )
    => Traversal' s Todo
    -> m ()
todo this = do
    bh "li" [("onRendered", onRendered)]
        [("className", classNames
            [("completed", model $ this._completed)
            ,("editing", model $ this._editing.to (== Editing))])]
        $ do
            (`runObserverT` onStartEdit) $ todoView this
            todoInput this
  where
    -- hdlStartEdit :: Tagged "TodoStartEdit" () -> m ()
    onStartEdit (untag' @"TodoStartEdit" -> ()) = do
        -- this will change the CSS style to make the label visible
        -- so we can't focus until label has been rendered
        noisyMutate $ this._editing .= Focusing
    onRendered = mkHandler pure hdlRendered
    hdlRendered j = do
        e <- model $ this._editing
        case e of
            Focusing -> do
                quietMutate $ this._editing .= Editing
                t <- guardJust $ fromJS @DOM.HTMLElement j
                DOM.focus t
                v <- guardJustM $ fromJS @JSString <$> t `getProperty` "value"
                t `setProperty` ("selectionStart", toJS @Int 0)
                t `setProperty` ("selectionEnd", toJS $ J.length v)
            _ -> pure ()


