{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Todo where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Diverse.Profunctor
import Data.Generics.Product
import qualified Data.JSString as J
import Data.Maybe
import Data.Tagged
import qualified GHC.Generics as G
import qualified Glazier.React.Framework as F
import qualified JavaScript.Extras as JE

data TodoInfo = TodoInfo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    , autoFocusEdit :: Bool
    } deriving G.Generic

data TodoToggleComplete

todoToggleComplete ::
    ( F.MonadReactor m
    , HasItemTag' TodoToggleComplete [F.Listener] s
    , HasItem' TodoInfo s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged TodoToggleComplete [F.Listener]])
        (Which '[])
        (Which '[])
        (Which '[])
todoToggleComplete = F.widget @TodoToggleComplete "input"
    (\s ->
        [ ("key", "toggle")
        , ("className", "toggle")
        , ("type", "checkbox")
        , ("checked", JE.toJS' . completed $ s ^. F.model.item')])
    (F.nulPrototype { F.activator' = onChange })
  where
    onChange ::
        ( HasItemTag' TodoToggleComplete [F.Listener] s
        , HasItem' TodoInfo s
        , F.MonadReactor m
        ) => F.SceneActivator m v s (Which '[])
    onChange = F.controlledTrigger @TodoToggleComplete
            "onChange"
            (const $ pure ())
            hdlChange

    hdlChange ::
        (F.MonadReactor m, HasItem' TodoInfo s)
        => F.SceneHandler m v s () (Which '[])
    hdlChange this@(F.Obj ref its) _ = F.terminate' . lift $ do
        F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"completed" %~ not)
        F.rerender' this

data TodoDestroy = TodoDestroy deriving (G.Generic, NFData)
todoDestroy ::
    ( F.MonadReactor m
    , HasItemTag' TodoDestroy [F.Listener] s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged TodoDestroy [F.Listener]])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
todoDestroy = F.widget @TodoDestroy "button"
    (const
        [ ("key", "destroy")
        , ("className", "destroy")])
    (F.nulPrototype { F.activator' = onClick })
  where
    onClick ::
        ( HasItemTag' TodoDestroy [F.Listener] s
        , F.MonadReactor m
        ) => F.SceneActivator m v s (Which '[TodoDestroy])
    onClick = F.trigger @TodoDestroy
            "onClick"
            (const . pure $ pickOnly TodoDestroy)

data TodoLabel
data TodoStartEdit = TodoStartEdit deriving (G.Generic, NFData)

todoLabel ::
    ( F.MonadReactor m
    , HasItemTag' TodoLabel [F.Listener] s
    , HasItem' TodoInfo s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged TodoLabel [F.Listener]])
        (Which '[TodoStartEdit])
        (Which '[])
        (Which '[])
todoLabel = F.widget @TodoLabel "label"
    (const [ ("key", "label")])
    (F.nulPrototype
        { F.display' = dis
        , F.activator' = onDoubleClick
        })
  where
    dis ::
        ( HasItem' TodoInfo s
        , F.MonadReactor m
        ) => F.FrameDisplay m s ()
    dis s = F.txt (s^.F.model.item' @TodoInfo .field @"value")

    onDoubleClick ::
        ( HasItemTag' TodoLabel [F.Listener] s
        , F.MonadReactor m
        ) => F.SceneActivator m v s (Which '[TodoStartEdit])
    onDoubleClick = F.trigger @TodoLabel
            "onDoubleClick"
            (const . pure $ pickOnly TodoStartEdit)

todoView ::
    ( F.MonadReactor m
    , HasItemTag' TodoToggleComplete [F.Listener] s
    , HasItemTag' TodoDestroy [F.Listener] s
    , HasItemTag' TodoLabel [F.Listener] s
    , HasItem' TodoInfo s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many
            '[Tagged TodoToggleComplete [F.Listener]
            , Tagged TodoDestroy [F.Listener]
            , Tagged TodoLabel [F.Listener]])
        (Which '[TodoDestroy, TodoStartEdit])
        (Which '[])
        (Which '[])
todoView =
    let p = todoToggleComplete
            `F.andPrototype` todoDestroy
            `F.andPrototype` todoLabel
        disp = F.display' p
    in p { F.display' = \s -> F.branch "div" []
                [ ("key", "view")
                , ("className", "view")]
                (disp s) }

data TodoInput
data TodoCancelEdit = TodoCancelEdit deriving (G.Generic, NFData)

todoInput ::
    ( F.MonadReactor m
    , F.MonadJS m
    , F.MonadHTMLElement m
    , HasItemTag' TodoInput [F.Listener] s
    , HasItemTag' TodoInput F.EventTarget s
    , HasItem' TodoInfo s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged TodoInput [F.Listener], Tagged TodoInput F.EventTarget])
        (Which '[TodoDestroy])
        (Which '[TodoStartEdit])
        (Which '[])
todoInput = F.widget @TodoInput "input"
    (\s ->
        -- For uncontrolled components, we need to generate a new key per render
        -- in order for react to use the new defaultValue
        [ ("key", JE.toJS' $ J.unwords
            [ F.runReactKey . F.reactKey $ s ^. F.plan
            , J.pack . show . F.frameNum $ s ^. F.plan
            ])
        , ("className", "edit")
        , ("defaultValue", JE.toJS' . value $ s ^. F.model.item')
        , ("defaultChecked", JE.toJS' . completed $ s ^. F.model.item')])
    (F.withRef @TodoInput `F.andPrototype` F.nulPrototype
        { F.activator' = onBlur `F.andActivator` onKeyDown
        , F.handler' = (. obvious) <$> hdlStartEdit })
  where
    onBlur ::
        ( HasItemTag' TodoInput [F.Listener] s
        , HasItem' TodoInfo s
        , F.MonadReactor m
        ) => F.SceneActivator m v s (Which '[])
    onBlur = F.controlledTrigger @TodoInput
            "onBlur"
            (const $ pure TodoCancelEdit)
            hdlBlur

    hdlBlur ::
        (F.MonadReactor m, HasItem' TodoInfo s)
        => F.SceneHandler m v s TodoCancelEdit (Which '[])
    hdlBlur this@(F.Obj ref its) _ = F.terminate' . lift $ do
        F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"completed" %~ not)
        F.rerender' this

    onKeyDown ::
        ( F.MonadReactor m
        , F.MonadJS m
        , HasItemTag' TodoInput [F.Listener] s
        , HasItem' TodoInfo s
        ) => F.SceneActivator m v s (Which '[TodoDestroy])
    onKeyDown = F.controlledTrigger @TodoInput
            "onKeyDown"
            (runMaybeT . F.fireKeyDownKey)
            (F.maybeHandle hdlKeyDown)

    hdlKeyDown ::
        ( F.MonadReactor m
        , F.MonadJS m
        , HasItem' TodoInfo s
        )
        => F.SceneHandler m v s F.KeyDownKey (Which '[TodoDestroy])
    hdlKeyDown this@(F.Obj ref its) (F.KeyDownKey j key) = ContT $ \fire ->
        case key of
            "Enter" -> do
                v <- JE.fromJS' @J.JSString <$> (F.doGetProperty "value" (JE.toJS j))
                let v' = J.strip $ fromMaybe J.empty v
                if J.null v'
                    then
                        fire $ pickOnly TodoDestroy
                    else do
                        F.doModifyIORef' ref (\s -> s
                            & its.F.model.item' @TodoInfo .field @"editing" .~ False
                            & its.F.model.item' @TodoInfo .field @"value" .~ v')
                        F.rerender this (F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j))
            "Escape" -> do
                F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"editing" .~ False)
                -- don't strictly need to reset the value, editing = false will hide this input
                -- but it's nice to reset the the dom
                F.rerender this (F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j))
            _ -> pure ()

    hdlStartEdit ::
        ( F.MonadReactor m
        , F.MonadJS m
        , F.MonadHTMLElement m
        , HasItem' TodoInfo s
        , HasItemTag' TodoInput F.EventTarget s)
        => F.SceneHandler m v s TodoStartEdit (Which '[])
    hdlStartEdit this@(F.Obj ref its) _ = F.terminate' $ lift $ do
        void $ runMaybeT $ do
            obj <- lift $ F.doReadIORef ref
            let b = obj ^. its.F.model.item' @TodoInfo .field @"completed"
            guard (not b)
            -- Focus after rendering changed because we are using uncontrollec components
            -- with a new key. This will result in a different input element after each render
            lift $ F.doModifyIORef' ref (\s -> s
                & its.F.model.item' @TodoInfo .field @"editing" .~ True
                )
            lift . F.rerender this $ do
                obj' <- F.doReadIORef ref
                let j = obj' ^. its.F.model.itemTag' @TodoInput @F.EventTarget
                F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j)
                F.focusRef @TodoInput this

todo ::
    ( F.MonadReactor m
    , F.MonadJS m
    , F.MonadHTMLElement m
    , HasItemTag' TodoDestroy [F.Listener] s
    , HasItemTag' TodoLabel [F.Listener] s
    , HasItemTag' TodoToggleComplete [F.Listener] s
    , HasItemTag' TodoInput F.EventTarget s
    , HasItemTag' TodoInput [F.Listener] s
    , HasItem' TodoInfo s
    , HasItem' TodoInfo i
    ) =>
    F.Prototype m v i s
        (Many '[TodoInfo])
        (Many
            '[TodoInfo
            , Tagged TodoToggleComplete [F.Listener]
            , Tagged TodoDestroy [F.Listener]
            , Tagged TodoLabel [F.Listener]
            , Tagged TodoInput [F.Listener]
            , Tagged TodoInput F.EventTarget])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
todo =
    let p = todoView `F.andPrototype` todoInput
        bld = F.builder' p
        act = F.activator' p
        hdl = F.handler' p
        disp = F.display' p
    in p { F.builder' = F.buildItem @TodoInfo `F.andBuilder` bld
        , F.activator' = act `F.drives'` hdl
        , F.handler' = F.nulHandler
        , F.display' = \s ->
            let s' = s ^. F.model.item' @TodoInfo
            in F.branch "div" []
                [ ("className", JE.classNames
                    [ ("completed", completed s')
                    , ("editing", editing s')])
                ]
                (disp s)
        }
