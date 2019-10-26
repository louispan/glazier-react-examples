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
import qualified GHC.Generics as G
import qualified Glazier.DOM as DOM
import Glazier.React
import Glazier.React.Widgets.Input

default (JSString)

-- data Editing = NotEditing | Editing | Focusing DOM.HTMLElement
--     deriving (Show, G.Generic)

data Todo = Todo
    { value :: JSString
    , completed :: Bool
    , editing :: Bool
    } deriving (Show, G.Generic)

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
    (MonadWidget s m, MonadObserver' (Tagged "TodoToggleComplete" ()) m)
    => Traversal' s Todo -> m ()
todoToggleComplete this = checkbox (this._completed)
    [("onChange", onChange)] [("className", "toggle")]
  where
    onChange = mkHandler' (const $ pure ()) (const $ observe' $ Tagged @"TodoToggleComplete" ())

todoDestroy :: (MonadWidget s m, MonadObserver' (Tagged "TodoDestroy" ()) m) => m ()
todoDestroy = lf "button" [("onClick", onClick)]
    [("key", "destroy"),("className", "destroy")]
  where
    onClick = mkHandler' (const $ pure ()) $
        const $ observe' $ Tagged @"TodoDestroy" ()

todoLabel :: (MonadWidget s m, MonadObserver' (Tagged "TodoStartEdit" DOM.HTMLElement) m)
    => Traversal' s Todo -> m ()
todoLabel this = bh "label" [("onDoubleClick", onDoubleClick)] []
    (txt (premodel $ this._value))
  where
    fromDoubleClick = fromJustM . pure . viaJS @DOM.HTMLElement . DOM.target
    onDoubleClick = mkHandler' fromDoubleClick $ \t ->
        observe' $ Tagged @"TodoStartEdit" t

todoInput ::
    ( MonadWidget s m
    , MonadObserver' (Tagged "InputChange" ()) m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    )
    => Traversal' s Todo
    -> m ()
todoInput this = input (this._value)
    [("onBlur", onBlur), ("onKeyDown", onKeyDown), ("onChange", onChange)]
    [("className", "edit")]
  where
    onChange = mkHandler' (const $ pure ()) (const $ observe' $ Tagged @"InputChange" ())
    onBlur = mkHandler' (const $ pure ()) (const $
        noisyMutate $ do
            this._editing .= False
            this._value %= J.strip)

    onKeyDown = mkHandler' fromKeyDown hdlKeyDown
    fromKeyDown evt = fromJustM $ pure $ (\t' e' -> (t', DOM.key e')) <$> t <*> e
      where
        e = viaJS @DOM.SyntheticKeyboardEvent evt
        t = viaJS @DOM.HTMLElement $ DOM.target evt
    hdlKeyDown (t, k) = case k of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with input onChange
            -- updating the value under our feet.
            "Enter" -> do
                    a <- fromJustM $ noisyMutate $ do
                        v <- use (this._value)
                        let v' = J.strip v
                        if J.null v'
                            then
                                pure $ Just $ Tagged @"TodoDestroy" ()
                            else do
                                this._editing .= False
                                this._value .= v'
                                pure Nothing
                    observe' a

            "Escape" -> DOM.blur t -- The onBlur handler will trim the value

            _ -> pure ()

todoView ::
    ( MonadWidget s m
    , MonadObserver' (Tagged "TodoToggleComplete" ()) m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    , MonadObserver' (Tagged "TodoStartEdit" DOM.HTMLElement) m
    )
    => Traversal' s Todo
    -> m ()
todoView this = do
    bh "div" [] [("className", "view")] $ todoToggleComplete this
    todoLabel this
    todoDestroy

todo ::
    ( MonadWidget s m
    , MonadObserver' (Tagged "TodoToggleComplete" ()) m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    , MonadObserver' (Tagged "InputChange" ()) m
    )
    => ReactId
    -> Traversal' s Todo
    -> m ()
todo scratchId this = do
    initRendered onRendered
    bh "li" []
        [("className", classNames
            [("completed", premodel $ this._completed)
            ,("editing", premodel $ this._editing)])]
        $ do
            todoView'
            todoInput this
  where
    todoView' = (`runObserverT` hdlStartEdit) $ todoView this

    -- hdlStartEdit :: Tagged "TodoStartEdit" () -> m ()
    hdlStartEdit (untag' @"TodoStartEdit" @DOM.HTMLElement -> t) = do
        setScratch scratchId "todo" t
        -- this will change the CSS style to make the label editable
        noisyMutate $ this._editing .= True
    onRendered = do
        t <- fromJustM $ fromJS @DOM.HTMLElement <$> getScratch scratchId "todo"
        deleteScratch scratchId "todo"
        -- we can only focus after the label become visible after CSS rerender
        DOM.focus t
        v <- fromJustM $ fromJS @JSString <$> t `getProperty` "value"
        t `setProperty` ("selectionStart", toJS @Int 0)
        t `setProperty` ("selectionEnd", toJS $ J.length v)


