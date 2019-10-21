{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Todo.Todo where
    -- ( Todo(..)
    -- , _value
    -- , _completed
    -- , _editing
    -- , TodoDestroy
    -- , TodoToggleComplete
    -- , todo
    -- ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Applicative as A
-- import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Action.KeyDownKey
import Glazier.React.Effect.HTMLElement
import Glazier.React.Effect.JavaScript
import qualified Glazier.React.Widgets.Input as W

data Todo = Todo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    } deriving (Show, Eq, Ord, G.Generic)

makeLenses_ ''Todo

instance A.ToJSON Todo where toEncoding = A.genericToEncoding A.defaultOptions
instance Applicative m => A.AToJSON m Todo
-- instance A.FromJSON Todo
-- instance Applicative m => A.AFromJSON m Todo

-- type OnTodoDestroy = Tagged "TodoDestroy"
-- type OnTodoToggleComplete = Tagged "TodoToggleComplete"
-- type OnTodoStartEdit = Tagged "TodoStartEdit"
-- data TodoDestroy = TodoDestroy
-- data TodoToggleComplete = TodoToggleComplete
-- data TodoStartEdit = TodoStartEdit

todoToggleComplete ::
    ( HasCallStack
    , AsReactor c
    , MonadWidget c s m
    , Observer (Tagged "TodoToggleComplete" ()) m
    )
    => Traversal' s Todo
    -> Widget m ()
todoToggleComplete this = reobserve (retag' @"InputChange" @"TodoToggleComplete" @()) `fmap2`
    W.checkboxInput (this._completed) [] [("className", strProp "toggle")]

todoDestroy ::
    ( HasCallStack
    , AsReactor c
    , MonadWidget c s m
    , Observer (Tagged "TodoDestroy" ()) m
    )
    => Widget m ()
todoDestroy =
    lf "button" [onClick]
        [("key", strProp "destroy")
        ,("className", strProp "destroy")]
  where
    onClick = trigger_ "onClick" (Tagged @"TodoDestroy" ()) observe

todoLabel ::
    ( HasCallStack
    , AsReactor c
    , MonadWidget c s m
    , Observer (Tagged "TodoStartEdit" ()) m
    )
    => Traversal' s Todo
    -> Widget m ()
todoLabel this = bh "label" [onDoubleClick] [] (rawTxt . view $ this._value)
  where
    onDoubleClick = trigger_ "onDoubleClick" (Tagged @"TodoStartEdit" ()) observe

todoInput ::
    ( HasCallStack
    , AsReactor c
    , AsJavascript c
    , AsHTMLElement c
    , MonadWidget c s m
    , Observer (Tagged "InputChange" ()) m
    , Observer (Tagged "TodoDestroy" ()) m
    )
    => Traversal' s Todo
    -> Widget m ()
todoInput this = W.textInput (this._value) [hdlBlur, hdlKeyDown]
    [("className", strProp "edit")]
  where
    hdlBlur = trigger_ "onBlur" () $ const $
        mutate $ do
            this._editing .= False
            this._value %= J.strip

    hdlKeyDown = trigger "onKeyDown" fireKeyDownKey $ \(KeyDownKey _ key) ->
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> mutateThen $ do
                v <- use (this._value)
                let v' = J.strip v
                if J.null v'
                    then
                        pure $ observe $ Tagged @"TodoDestroy" ()
                    else do
                        this._editing .= False
                        this._value .= v'
                        pure $ pure ()

            "Escape" -> do
                j <- getReactRef
                blur j -- The onBlur handler will trim the value

            _ -> pure ()


todoView ::
    ( HasCallStack
    , AsReactor c
    , MonadWidget c s m
    , Observer (Tagged "TodoToggleComplete" ()) m
    , Observer (Tagged "TodoDestroy" ()) m
    , Observer (Tagged "TodoStartEdit" ()) m
    )
    => Traversal' s Todo
    -> Widget m ()
todoView this = bh "div" [] [("className", strProp "view")] $
    todoToggleComplete this
    <> todoLabel this
    <> todoDestroy

todo ::
    ( HasCallStack
    , AsReactor c
    , AsJavascript c
    , AsHTMLElement c
    , MonadWidget c s m
    , Observer (Tagged "TodoToggleComplete" ()) m
    , Observer (Tagged "TodoDestroy" ()) m
    , Observer (Tagged "InputChange" ()) m
    )
    => Traversal' s Todo
    -> Widget m ()
todo this = bh "li" [] [("className", classNames
                        [("completed", fromJustM $ preview (this._completed))
                        ,("editing", fromJustM $ preview (this._editing))])]
                (todoView' <> todoInput this)
  where
    todoView' = (`runObserver` hdlStartEdit) `fmap2` todoView this
    -- hdlStartEdit :: Tagged "TodoStartEdit" () -> m ()
    hdlStartEdit (untag @"TodoStartEdit" -> ()) = do
        mutate $ this._editing .= True
        j <- getReactRef
        onRenderedOnce $ do
            focus j
            v <- fromProperty "value" j
            let i = J.length v
            setProperty ("selectionStart", (0 :: Int)) j
            setProperty ("selectionEnd", i) j

