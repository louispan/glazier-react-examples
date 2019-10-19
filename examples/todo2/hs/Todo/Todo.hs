{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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

-- import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified Glazier.DOM as DOM
import Glazier.React
import Glazier.React.Widgets.Input

default (JSString)

data Editing = NotEditing | Editing | Focusing DOM.HTMLElement
    deriving (Show, G.Generic)

data Todo = Todo
    { value :: JSString
    , completed :: Bool
    , editing :: Editing
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
todoToggleComplete this = checkbox (Tagged @"TodoToggleComplete" ()) (this._completed)
    [] [("className", strProp "toggle")]

todoDestroy :: (MonadWidget s m, MonadObserver' (Tagged "TodoDestroy" ()) m) => m ()
todoDestroy = lf "button" [("onClick", onClick)]
    [("key", strProp "destroy"),("className", strProp "destroy")]
  where
    onClick = mkSyntheticHandler (const $ pure ()) $
        const $ observe' $ Tagged @"TodoDestroy" ()

todoLabel :: (MonadWidget s m, MonadObserver' (Tagged "TodoStartEdit" DOM.HTMLElement) m)
    => Traversal' s Todo -> m ()
todoLabel this = bh (jsstr "label") [("onDoubleClick", onDoubleClick)] []
    (strTxt (view $ this._value))
  where
    fromDoubleClick = maybeM . pure . viaJS @DOM.HTMLElement . DOM.target
    onDoubleClick = mkSyntheticHandler fromDoubleClick $ \t ->
        observe' $ Tagged @"TodoStartEdit" t

todoInput ::
    ( MonadWidget s m
    , MonadObserver' (Tagged "InputChange" ()) m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    )
    => Traversal' s Todo
    -> m ()
todoInput this = input (Tagged @"InputChange" ()) (this._value)
    [("onBlur", onBlur), ("onKeyDown", onKeyDown)]
    [("className", strProp "edit")]
  where
    onBlur = mkSyntheticHandler (const $ pure ()) (const $
        mutate' $ do
            this._editing .= NotEditing
            this._value %= J.strip)

    onKeyDown = mkSyntheticHandler fromKeyDown hdlKeyDown
    fromKeyDown evt = maybeM $ pure $ (\t' e' -> (t', DOM.key e')) <$> t <*> e
      where
        e = viaJS @DOM.SyntheticKeyboardEvent evt
        t = viaJS @DOM.HTMLElement $ DOM.target evt
    hdlKeyDown (t, k) = case k of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with input onChange
            -- updating the value under our feet.
            "Enter" -> do
                    a <- maybeM $ mutate' $ do
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
    , MonadObserver' (Tagged "TodoToggleComplete" ()) m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    , MonadObserver' (Tagged "TodoStartEdit" DOM.HTMLElement) m
    )
    => Traversal' s Todo
    -> m ()
todoView this = do
    bh "div" [] [("className", strProp "view")] $ todoToggleComplete this
    todoLabel this
    todoDestroy

todo ::
    ( MonadWidget s m
    , MonadObserver' (Tagged "TodoToggleComplete" ()) m
    , MonadObserver' (Tagged "TodoDestroy" ()) m
    , MonadObserver' (Tagged "InputChange" ()) m
    )
    => Traversal' s Todo
    -> m ()
todo this = do
    initRendered onRendered
    bh "li" []
        [("className", classNames
            [("completed", preview $ this._completed)
            ,("editing", preview $ this._editing.to isEditing)])]
        $ do
            todoView'
            todoInput this
  where
    isEditing NotEditing = False
    isEditing _ = True

    todoView' = (`runObserverT` hdlStartEdit) $ todoView this

    -- hdlStartEdit :: Tagged "TodoStartEdit" () -> m ()
    hdlStartEdit (untag' @"TodoStartEdit" @DOM.HTMLElement -> t) =
        -- this will change the CSS style to make the label editable
        mutate' $ this._editing .= Focusing t

    onRendered = do
        s <- askModel
        e <- maybeM $ pure $ preview (this._editing) s
        case e of
            Focusing t -> do
                mutate RerenderNotRequired $ this._editing .= Editing
                -- we can only focus after the label become visible after CSS rerender
                DOM.focus t
                v <- maybeM $ fromJS @JSString <$> getProperty t "value"
                setProperty t ("selectionStart", toJS @Int 0)
                setProperty t ("selectionEnd", toJS $ J.length v)
            _ -> pure ()


