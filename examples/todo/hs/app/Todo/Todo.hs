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
import qualified Glazier.React.Framework as R
import qualified JavaScript.Extras as JE

data TodoInfo = TodoInfo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    } deriving G.Generic

todoToggleComplete ::
    ( R.MonadReactor m
    )
    => R.Prototype m v i TodoInfo
        (Many '[])
        (Many '[])
        (Which '[])
        (Which '[])
        (Which '[])
todoToggleComplete = R.nulPrototype
        { R.display = \s -> R.lf'' i s "input"
                [ ("key", "toggle")
                , ("className", "toggle")
                , ("type", "checkbox")
                , ("checked", JE.toJS' . completed $ s ^. R.model)]
        , R.activator = onChange }
  where
    i = R.GadgetId "toggle"
    onChange ::
        ( R.MonadReactor m
        ) => R.SceneActivator m v TodoInfo (Which '[])
    onChange = R.trigger' i "onChange" (const $ pure ())
            `R.activates` hdlChange

    hdlChange ::
        (R.MonadReactor m)
        => R.SceneHandler m v TodoInfo () (Which '[])
    hdlChange this@(R.Obj ref its) _ = R.terminate' . lift $ do
        R.doModifyIORef' ref (its.R.model.field @"completed" %~ not)
        R.rerender' this

data TodoDestroy = TodoDestroy

todoDestroy ::
    ( R.MonadReactor m
    )
    => R.Prototype m v i s
        (Many '[])
        (Many '[])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
todoDestroy = R.nulPrototype
    { R.display = \s -> R.lf'' i s "button"
            [ ("key", "destroy")
            , ("className", "destroy")]
    , R.activator = onClick }
  where
    i = R.GadgetId "destroy"
    onClick :: (R.MonadReactor m) => R.SceneActivator m v s (Which '[TodoDestroy])
    onClick = R.trigger i "onClick"
            (const $ pure ()) (const $ pickOnly TodoDestroy)

data TodoStartEdit = TodoStartEdit

todoLabel ::
    ( R.MonadReactor m
    )
    => R.Prototype m v i TodoInfo
        (Many '[])
        (Many '[])
        (Which '[TodoStartEdit])
        (Which '[])
        (Which '[])
todoLabel = R.nulPrototype
        { R.display = disp
        , R.activator = onDoubleClick
        }
  where
    i = R.GadgetId "label"
    disp ::
        ( R.MonadReactor m
        ) => R.FrameDisplay m TodoInfo ()
    disp s = R.bh'' i s "label" [("key", "label")] $
                R.txt (s ^. R.model.field @"value")
    onDoubleClick :: (R.MonadReactor m) => R.SceneActivator m v s (Which '[TodoStartEdit])
    onDoubleClick = R.trigger i "onDoubleClick"
            (const $ pure ()) (const $ pickOnly TodoStartEdit)

todoView ::
    ( R.MonadReactor m)
    => R.Prototype m v i TodoInfo
        (Many '[])
        (Many '[])
        (Which '[TodoDestroy, TodoStartEdit])
        (Which '[])
        (Which '[])
todoView =
    let p = todoToggleComplete
            `R.andPrototype` todoDestroy
            `R.andPrototype` todoLabel
        disp = R.display p
    in p { R.display = \s -> R.bh "div"
                [ ("key", "view")
                , ("className", "view")]
                (disp s) }

todoInput ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    )
    => R.Prototype m v i TodoInfo
        (Many '[])
        (Many '[])
        (Which '[TodoDestroy])
        (Which '[TodoStartEdit])
        (Which '[])
todoInput = R.nulPrototype
    { R.display = \s -> R.lf'' i s "input"
            -- For uncontrolled components, we need to generate a new key per render
            -- in order for react to use the new defaultValue
            [ ("key", JE.toJS' $ J.unwords
                [ R.runReactKey . R.reactKey $ s ^. R.plan
                , J.pack . show . R.frameNum $ s ^. R.plan
                ])
            , ("className", "edit")
            , ("defaultValue", JE.toJS' . value $ s ^. R.model)
            , ("defaultChecked", JE.toJS' . completed $ s ^. R.model)]
    , R.activator = R.withRef i `R.andActivator` onBlur `R.andActivator` onKeyDown
    , R.handler = (. obvious) <$> hdlStartEdit }
  where
    i = R.GadgetId "input"
    onBlur ::
        ( R.MonadReactor m, R.MonadHTMLElement m
        ) => R.SceneActivator m v TodoInfo (Which '[])
    onBlur = R.trigger' i "onBlur" (const $ pure ())
            `R.activates` hdlBlur

    hdlBlur :: (R.MonadReactor m, R.MonadHTMLElement m)
        => R.SceneHandler m v TodoInfo () (Which '[])
    hdlBlur this@(R.Obj ref its) _ = R.terminate' . lift $ do
        R.doModifyIORef' ref (its.R.model.field @"completed" %~ not)
        R.blurRef i this
        R.rerender' this

    onKeyDown ::
        ( R.MonadReactor m
        , R.MonadJS m
        ) => R.SceneActivator m v TodoInfo (Which '[TodoDestroy])
    onKeyDown = R.trigger' i "onKeyDown" (runMaybeT . R.fireKeyDownKey)
            `R.activates` R.maybeHandle hdlKeyDown

    hdlKeyDown ::
        ( R.MonadReactor m
        , R.MonadJS m
        )
        => R.SceneHandler m v TodoInfo R.KeyDownKey (Which '[TodoDestroy])
    hdlKeyDown this@(R.Obj ref its) (R.KeyDownKey j key) = ContT $ \fire ->
        case key of
            "Enter" -> do
                v <- JE.fromJS' @J.JSString <$> (R.doGetProperty "value" (JE.toJS j))
                let v' = J.strip $ fromMaybe J.empty v
                if J.null v'
                    then
                        fire $ pickOnly TodoDestroy
                    else do
                        R.doModifyIORef' ref (\s -> s
                            & its.R.model.field @"editing" .~ False
                            & its.R.model.field @"value" .~ v')
                        R.rerender this (R.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j))
            "Escape" -> do
                R.doModifyIORef' ref (its.R.model.field @"editing" .~ False)
                -- don't strictly need to reset the value, editing = false will hide this input
                -- but it's nice to reset the the dom
                R.rerender this (R.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j))
            _ -> pure ()

    hdlStartEdit ::
        ( R.MonadReactor m
        , R.MonadJS m
        , R.MonadHTMLElement m)
        => R.SceneHandler m v TodoInfo TodoStartEdit (Which '[])
    hdlStartEdit this@(R.Obj ref its) _ = R.terminate' $ lift $ do
        void $ runMaybeT $ do
            obj <- lift $ R.doReadIORef ref
            let b = obj ^. its.R.model.field @"completed"
            guard (not b)
            -- Focus after rendering changed because we are using uncontrollec components
            -- with a new key. This will result in a different input element after each render
            lift $ R.doModifyIORef' ref (\s -> s
                & its.R.model.field @"editing" .~ True
                )
            lift . R.rerender this $ do
                obj' <- R.doReadIORef ref
                let j = obj' ^. its.R.plan.field @"refs".at i
                case j of
                    Nothing -> pure ()
                    Just j' -> do
                        R.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j')
                        R.focusRef i this

todo ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    , HasItem' TodoInfo s
    , HasItem' TodoInfo i
    ) =>
    R.Prototype m v i s
        (Many '[TodoInfo])
        (Many '[TodoInfo])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
todo =
    let p = todoView `R.andPrototype` todoInput
        hdl = R.handler p
        p' = R.mapBuilder (R.constBuilder $ R.build @TodoInfo) $
            R.mapActivator (`R.activates'` hdl) $
            R.mapDisplay (\disp s ->
                let s' = s ^. R.model
                    in R.bh "div"
                        [ ("className", JE.classNames
                            [ ("completed", completed s')
                            , ("editing", editing s')])
                        ]
                        (disp s)) $
            p { R.handler = R.nulHandler } -- don't need to expose StartEdit handler
    in R.toItemPrototype p'
