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
-- {-# LANGUAGE ViewPatterns #-}
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
    W.checkboxInput (this._completed) [("className", strProp "toggle")] []

todoDestroy ::
    ( HasCallStack
    , AsReactor c
    , MonadWidget c s m
    , Observer (Tagged "TodoDestroy" ()) m
    )
    => Widget m ()
todoDestroy =
    lf "button"
        [("key", strProp "destroy")
        ,("className", strProp"destroy")]
        [onClick]
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
todoLabel this = bh "label" [] [onDoubleClick] (rawTxt . view $ this._value)
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
todoInput this = W.textInput (this._value)
    [("className", strProp "edit")]
    [hdlBlur, hdlKeyDown]
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
todoView this = bh "div" [("className", strProp "view")] [] $
    todoToggleComplete this
    <> todoLabel this
    <> todoDestroy

-- todo :: (AsReactor c, AsJavascript c, AsHTMLElement c)
--     => Widget c o Todo (Which '[OnTodoToggleComplete ReactId, OnTodoDestroy ReactId])

-- todo ::
--     ( HasCallStack
--     , AsReactor c
--     , AsJavascript c
--     , AsHTMLElement c
--     , MonadWidget c s m
--     , Observer (Tagged "TodoToggleComplete" ()) m
--     , Observer (Tagged "TodoDestroy" ()) m
--     )
--     => Traversal' s Todo
-- todo this = do
--     k <- askReactId
--     -- prepare to run the children with a locally scoped modified reactid, pushing this name in the list of names
--     modifyReactId $ \(ReactId (ns, _)) -> ReactId (mempty NE.<| ns, 0)
--     -- restore this key
--     putReactId k




--     k <- mkReactId "input"
--     let todoInput' = pickOnly <$> todoInput k
--         wid = overWindow2' (*>) todoView todoInput'
--     overWindow fw wid >>= (injectedK $ lift . totally . finish . hdlStartEdit k . obvious)

--   where
--     fw win = do
--         s <- view _model
--         bh "li"
--             [ ("className", JE.classNames
--                 [ ("completed", completed s)
--                 , ("editing", editing s)])
--             ]
--             win

--     hdlStartEdit :: (AsHTMLElement c, AsJavascript c, AsReactor c)
--         => ReactId -> OnTodoStartEdit ReactId -> Gadget c o Todo ()
--     hdlStartEdit k (untag @"OnTodoStartEdit" -> _) = do
--         mutate k $ _editing .= True
--         j <- getReactRef k
--         onNextRendered $ do
--             focus j
--             (`evalMaybeT` ()) $ do
--                 v <- MaybeT $ JE.fromJSR <$> getProperty "value" j
--                 let i = J.length v
--                 setProperty ("selectionStart", JE.toJSR (0 :: Int)) j
--                 setProperty ("selectionEnd", JE.toJSR i) j
