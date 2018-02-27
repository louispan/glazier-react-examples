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
import qualified Glazier.React.Widget as W
import qualified JavaScript.Extras as JE

data TodoInfo = TodoInfo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    } deriving G.Generic

todoToggleComplete :: (R.MonadReactor m, R.MonadHTMLElement m) => R.Prototype m v TodoInfo (Which '[])
todoToggleComplete = W.checkboxInput (R.GadgetId "toggle")
    & R.modifyInitializer fini
    & R.modifyDisplay fdisp
    & R.magnifyPrototype (field @"completed")
 where
    fdisp disp s = R.modifySurfaceProperties fprops (disp s)
    fprops = (:) ("className", "toggle")
    fini ini = ini `R.handledBy` (R.ignoreHandler @(Which '[W.OnFocus, W.OnBlur, W.OnEsc, W.OnToggle]))

data TodoDestroy = TodoDestroy

todoDestroy :: (R.MonadReactor m) => R.Prototype m v s (Which '[TodoDestroy])
todoDestroy = R.nulPrototype
    { R.display = \s -> R.lf' i s "button"
            [ ("key", "destroy")
            , ("className", "destroy")]
    , R.initializer = onClick }
  where
    i = R.GadgetId "destroy"
    onClick :: (R.MonadReactor m) => R.SceneInitializer m v s (Which '[TodoDestroy])
    onClick = R.trigger i "onClick"
            (const $ pure ()) (const $ pickOnly TodoDestroy)

data TodoStartEdit = TodoStartEdit

todoLabel :: (R.MonadReactor m) => R.Prototype m v TodoInfo (Which '[TodoStartEdit])
todoLabel = R.nulPrototype
        { R.display = disp
        , R.initializer = onDoubleClick
        }
  where
    i = R.GadgetId "label"
    disp :: (R.MonadReactor m) => R.FrameDisplay m TodoInfo ()
    disp s = R.bh' i s "label" [("key", "label")] $
                R.txt (s ^. R.model.field @"value")
    onDoubleClick :: (R.MonadReactor m) => R.SceneInitializer m v s (Which '[TodoStartEdit])
    onDoubleClick = R.trigger i "onDoubleClick"
            (const $ pure ()) (const $ pickOnly TodoStartEdit)

todoView :: (R.MonadReactor m, R.MonadHTMLElement m) => R.Prototype m v TodoInfo (Which '[TodoDestroy, TodoStartEdit])
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
    => R.Prototype m v TodoInfo (Which '[TodoDestroy])
todoInput = (W.textInput (R.GadgetId "input"))
    & R.magnifyPrototype (field @"value")
    & R.modifyInitializer fini
    & R.modifyDisplay fdisp
  where
    fdisp disp s = R.modifySurfaceProperties fprops (disp s)
    fprops = (:) ("className", "edit")
    fini ini = ini
        `R.handledBy'` (R.ignoreHandler @(Which '[W.OnEsc]))
        `R.handledBy'` (R.obviousHandler hdlFocus)
        `R.handledBy'` (R.obviousHandler hdlBlur)
        `R.handledBy'` (R.obviousHandler hdlEnter)

    hdlFocus :: (R.MonadReactor m)
        => R.SceneHandler m v TodoInfo W.OnFocus (Which '[])
    hdlFocus (R.Obj ref its) _ = R.terminate' . lift $ do
        R.doModifyIORef' ref (its.R.model.field @"editing" .~ True)
        R.stale this

    hdlBlur :: (R.MonadReactor m, R.MonadJS m)
        => R.SceneHandler m v TodoInfo W.OnBlur (Which '[])
    hdlBlur (R.Obj ref its) (W.OnBlur _ j) = R.terminate' . lift $ do
        obj <- R.doReadIORef ref
        let v = obj ^. its.R.model.field @"value"
            v' = J.strip v
        R.doModifyIORef' ref (\s -> s
            & its.R.model.field @"editing" .~ False
            & its.R.model.field @"value" .~ v')
        R.doSetProperty ("value", JE.toJSR v') (JE.toJS j)
        R.stale this

    hdlEnter :: (R.MonadReactor m, R.MonadJS m)
        => R.SceneHandler m v TodoInfo W.OnEnter (Which '[TodoDestroy])
    hdlEnter (R.Obj ref its) (W.OnEnter _ j) = ContT $ \fire -> do
        obj <- R.doReadIORef ref
        let v = obj ^. its.R.model.field @"value"
            v' = J.strip v
        if J.null v'
            then
                fire $ pickOnly TodoDestroy
            else do
                R.doModifyIORef' ref (\s -> s
                    & its.R.model.field @"editing" .~ False
                    & its.R.model.field @"value" .~ v')
                R.doSetProperty ("value", JE.toJSR v') (JE.toJS j)
                R.stale this

hdlStartEdit ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m)
    => R.GadgetId -> R.SceneHandler m v TodoInfo TodoStartEdit (Which '[])
hdlStartEdit i this@(R.Obj ref its) _ = R.terminate' $ lift $ do
    void $ runMaybeT $ do
        obj <- lift $ R.doReadIORef ref
        let b = obj ^. its.R.model.field @"completed"
        guard (not b)
        -- Focus after rendering changed because we are using uncontrollec components
        -- with a new key. This will result in a different input element after each render
        lift $ R.doModifyIORef' ref (\s -> s
            & its.R.model.field @"editing" .~ True
            )
        lift . R.addOnceOnUpdated this $ do
            obj' <- R.doReadIORef ref
            let j = obj' ^. its.R.plan.field @"refs".at i
            case j of
                Nothing -> pure ()
                Just j' -> do
                    R.doSetProperty ("value", JE.toJSR J.empty) (JE.toJS j')
                    R.focusRef i this
        lift $ R.stale this

todo ::
    ( R.MonadReactor m
    , R.MonadJS m
    , R.MonadHTMLElement m
    ) => R.Prototype m v TodoInfo (Which '[TodoDestroy])
todo =
    let p = todoView `R.andPrototype` todoInput
    in p & R.modifyDisplay fdisp
        & R.modifyInitializer (`R.handledBy'` (R.obviousHandler $ hdlStartEdit (R.GadgetId "input")))
  where
    fdisp disp s =
        let s' = s ^. R.model
        in R.bh "div"
            [ ("className", JE.classNames
                [ ("completed", completed s')
                , ("editing", editing s')])
            ]
            (disp s)
