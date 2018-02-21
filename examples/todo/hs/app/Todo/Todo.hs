{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Todo where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Diverse.Profunctor
import Data.Generics.Product
import qualified Data.JSString as J
import Data.Maybe
import qualified GHC.Generics as G
import qualified Glazier.React.Framework as F
import qualified JavaScript.Extras as JE

data TodoInfo = TodoInfo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    , autoFocusEdit :: Bool
    } deriving G.Generic

todoToggleComplete ::
    ( F.MonadReactor m
    )
    => F.Prototype m v i TodoInfo
        (Many '[])
        (Many '[])
        (Which '[])
        (Which '[])
        (Which '[])
todoToggleComplete = F.nulPrototype
        { F.display = \s -> F.leaf "input" (F.getListeners i s)
                [ ("key", "toggle")
                , ("className", "toggle")
                , ("type", "checkbox")
                , ("checked", JE.toJS' . completed $ s ^. F.model)]
        , F.activator = onChange }
  where
    i = F.GadgetId "toggle"
    onChange ::
        ( F.MonadReactor m
        ) => F.SceneActivator m v TodoInfo (Which '[])
    onChange = F.trigger' i "onChange" (const $ pure ())
            `F.activates` hdlChange

    hdlChange ::
        (F.MonadReactor m)
        => F.SceneHandler m v TodoInfo () (Which '[])
    hdlChange this@(F.Obj ref its) _ = F.terminate' . lift $ do
        F.doModifyIORef' ref (its.F.model.field @"completed" %~ not)
        F.rerender' this

data TodoDestroy = TodoDestroy

todoDestroy ::
    ( F.MonadReactor m
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
todoDestroy = F.nulPrototype
    { F.display = \s -> F.leaf "button" (F.getListeners i s)
            [ ("key", "destroy")
            , ("className", "destroy")]
    , F.activator = onClick }
  where
    i = F.GadgetId "destroy"
    onClick :: (F.MonadReactor m) => F.SceneActivator m v s (Which '[TodoDestroy])
    onClick = F.trigger i "onClick"
            (const $ pure ()) (const $ pickOnly TodoDestroy)

data TodoStartEdit = TodoStartEdit

todoLabel ::
    ( F.MonadReactor m
    )
    => F.Prototype m v i TodoInfo
        (Many '[])
        (Many '[])
        (Which '[TodoStartEdit])
        (Which '[])
        (Which '[])
todoLabel = F.nulPrototype
        { F.display = disp
        , F.activator = onDoubleClick
        }
  where
    i = F.GadgetId "label"
    disp ::
        ( F.MonadReactor m
        ) => F.FrameDisplay m TodoInfo ()
    disp s = F.branch "label" (F.getListeners i s) [("key", "label")] $
                F.txt (s ^. F.model.field @"value")
    onDoubleClick :: (F.MonadReactor m) => F.SceneActivator m v s (Which '[TodoStartEdit])
    onDoubleClick = F.trigger i "onDoubleClick"
            (const $ pure ()) (const $ pickOnly TodoStartEdit)

todoView ::
    ( F.MonadReactor m)
    => F.Prototype m v i TodoInfo
        (Many '[])
        (Many '[])
        (Which '[TodoDestroy, TodoStartEdit])
        (Which '[])
        (Which '[])
todoView =
    let p = todoToggleComplete
            `F.andPrototype` todoDestroy
            `F.andPrototype` todoLabel
        disp = F.display p
    in p { F.display = \s -> F.branch' "div"
                [ ("key", "view")
                , ("className", "view")]
                (disp s) }

data TodoCancelEdit = TodoCancelEdit

todoInput ::
    ( F.MonadReactor m
    , F.MonadJS m
    , F.MonadHTMLElement m
    )
    => F.Prototype m v i TodoInfo
        (Many '[])
        (Many '[])
        (Which '[TodoDestroy])
        (Which '[TodoStartEdit])
        (Which '[])
todoInput = F.nulPrototype
    { F.display = \s -> F.leaf "input" (F.getListeners i s)
            -- For uncontrolled components, we need to generate a new key per render
            -- in order for react to use the new defaultValue
            [ ("key", JE.toJS' $ J.unwords
                [ F.runReactKey . F.reactKey $ s ^. F.plan
                , J.pack . show . F.frameNum $ s ^. F.plan
                ])
            , ("className", "edit")
            , ("defaultValue", JE.toJS' . value $ s ^. F.model)
            , ("defaultChecked", JE.toJS' . completed $ s ^. F.model)]
    , F.activator = F.withRef i `F.andActivator` onBlur `F.andActivator` onKeyDown
    , F.handler = (. obvious) <$> hdlStartEdit }
  where
    i = F.GadgetId "input"
    onBlur ::
        ( F.MonadReactor m
        ) => F.SceneActivator m v TodoInfo (Which '[])
    onBlur = F.trigger i "onBlur" (const $ pure ()) (const TodoCancelEdit)
            `F.activates` hdlBlur

    hdlBlur ::
        (F.MonadReactor m)
        => F.SceneHandler m v TodoInfo TodoCancelEdit (Which '[])
    hdlBlur this@(F.Obj ref its) _ = F.terminate' . lift $ do
        F.doModifyIORef' ref (its.F.model.field @"completed" %~ not)
        F.rerender' this

    onKeyDown ::
        ( F.MonadReactor m
        , F.MonadJS m
        ) => F.SceneActivator m v TodoInfo (Which '[TodoDestroy])
    onKeyDown = F.trigger' i "onKeyDown" (runMaybeT . F.fireKeyDownKey)
            `F.activates` F.maybeHandle hdlKeyDown

    hdlKeyDown ::
        ( F.MonadReactor m
        , F.MonadJS m
        )
        => F.SceneHandler m v TodoInfo F.KeyDownKey (Which '[TodoDestroy])
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
                            & its.F.model.field @"editing" .~ False
                            & its.F.model.field @"value" .~ v')
                        F.rerender this (F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j))
            "Escape" -> do
                F.doModifyIORef' ref (its.F.model.field @"editing" .~ False)
                -- don't strictly need to reset the value, editing = false will hide this input
                -- but it's nice to reset the the dom
                F.rerender this (F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j))
            _ -> pure ()

    hdlStartEdit ::
        ( F.MonadReactor m
        , F.MonadJS m
        , F.MonadHTMLElement m)
        => F.SceneHandler m v TodoInfo TodoStartEdit (Which '[])
    hdlStartEdit this@(F.Obj ref its) _ = F.terminate' $ lift $ do
        void $ runMaybeT $ do
            obj <- lift $ F.doReadIORef ref
            let b = obj ^. its.F.model.field @"completed"
            guard (not b)
            -- Focus after rendering changed because we are using uncontrollec components
            -- with a new key. This will result in a different input element after each render
            lift $ F.doModifyIORef' ref (\s -> s
                & its.F.model.field @"editing" .~ True
                )
            lift . F.rerender this $ do
                obj' <- F.doReadIORef ref
                let j = obj' ^. its.F.plan.field @"refs".at i
                case j of
                    Nothing -> pure ()
                    Just j' -> do
                        F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j')
                        F.focusRef i this

todo ::
    ( F.MonadReactor m
    , F.MonadJS m
    , F.MonadHTMLElement m
    , HasItem' TodoInfo s
    , HasItem' TodoInfo i
    ) =>
    F.Prototype m v i s
        (Many '[TodoInfo])
        (Many '[TodoInfo])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
todo =
    let p = todoView `F.andPrototype` todoInput
        act = F.activator p
        hdl = F.handler p
        disp = F.display p
        p' = p {
            F.builder = F.build @TodoInfo
            , F.activator = act `F.activates'` hdl
            , F.handler = F.nulHandler -- don't need to expose StartEdit handler
            , F.display = \s ->
                let s' = s ^. F.model
                in F.branch "div" []
                    [ ("className", JE.classNames
                        [ ("completed", completed s')
                        , ("editing", editing s')])
                    ]
                    (disp s)
            }
    in F.toItemPrototype p'
