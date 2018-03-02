{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
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
import qualified Data.DList as DL
import Data.Generics.Product
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified Glazier.React.Framework as Z
import qualified Glazier.React.Widget as W
import qualified JavaScript.Extras as JE

data TodoInfo = TodoInfo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    } deriving G.Generic

todoToggleComplete :: (Z.MonadReactor m) => Z.Prototype m v TodoInfo (Which '[])
todoToggleComplete = W.checkboxInput (Z.GadgetId "toggle")
    & Z.modifyDisplay fdisp
    & Z.magnifyPrototype (field @"completed")
 where
    fdisp disp s = Z.modifySurfaceProperties fprops (disp s)
    fprops = (`DL.snoc` ("className", "toggle"))

data TodoDestroy = TodoDestroy

todoDestroy :: (Z.MonadReactor m) => Z.Prototype m v s (Which '[TodoDestroy])
todoDestroy = Z.nulPrototype
    { Z.display = \s -> Z.lf' gid s "button"
            [ ("key", "destroy")
            , ("className", "destroy")]
    , Z.initializer = Z.trigger' gid "onClick" (pickOnly TodoDestroy)
    }
  where
    gid = Z.GadgetId "destroy"

data TodoStartEdit = TodoStartEdit

todoLabel :: (Z.MonadReactor m) => Z.Prototype m v TodoInfo (Which '[TodoStartEdit])
todoLabel = Z.nulPrototype
        { Z.display = disp
        , Z.initializer = Z.trigger' gid "onDoubleClick" (pickOnly TodoStartEdit)
        }
  where
    gid = Z.GadgetId "label"
    disp :: (Z.MonadReactor m) => Z.FrameDisplay m TodoInfo ()
    disp s = Z.bh' gid s "label" [("key", "label")] $
                Z.txt (s ^. Z.model.field @"value")

todoView :: (Z.MonadReactor m) => Z.Prototype m v TodoInfo (Which '[TodoDestroy, TodoStartEdit])
todoView =
    let p = todoToggleComplete
            `Z.alsoPrototype` todoDestroy
            `Z.alsoPrototype` todoLabel
        disp = Z.display p
    in p { Z.display = \s -> Z.bh "div"
                [ ("key", "view")
                , ("className", "view")]
                (disp s) }

todoInput ::
    ( Z.MonadReactor m
    , Z.MonadJS m
    , Z.MonadHTMLElement m
    )
    => Z.Prototype m v TodoInfo (Which '[TodoDestroy])
todoInput = (W.textInput gid)
    & Z.magnifyPrototype (field @"value")
    & Z.modifyInitializer fini
    & Z.modifyDisplay fdisp
  where
    gid = Z.GadgetId "input"
    fdisp disp s = Z.modifySurfaceProperties fprops (disp s)
    fprops = (`DL.snoc` ("className", "edit"))
    fini ini = ini
        `Z.alsoInitializer` (Z.trigger' gid "onFocus" () `Z.handledBy` hdlFocus)
        `Z.alsoInitializer` (Z.trigger' gid "onBlur" () `Z.handledBy` hdlBlur)
        `Z.alsoInitializer` (Z.trigger' gid "onBlur" () `Z.handledBy` hdlBlur)
        `Z.alsoInitializer` (Z.trigger gid "onKeyDown" (runMaybeT . Z.fireKeyDownKey) id
            `Z.handledBy` Z.maybeHandle hdlKeyDown)

    hdlFocus :: (Z.MonadReactor m)
        => Z.SceneHandler m v TodoInfo a (Which '[])
    hdlFocus this@(Z.Obj ref its) _ = Z.terminate' . lift $ do
        Z.doModifyIORef' ref (its.Z.model.field @"editing" .~ True)
        Z.dirty this

    hdlBlur :: (Z.MonadReactor m)
        => Z.SceneHandler m v TodoInfo a (Which '[])
    hdlBlur this@(Z.Obj ref its) _ = Z.terminate' . lift $ do
        Z.doModifyIORef' ref (\s -> s
            & its.Z.model.field @"editing" .~ False
            & its.Z.model.field @"value" %~ J.strip)
        Z.dirty this

    hdlKeyDown ::
        ( Z.MonadReactor m
        , Z.MonadHTMLElement m
        )
        => Z.SceneHandler m v TodoInfo Z.KeyDownKey (Which '[TodoDestroy])
    hdlKeyDown this@(Z.Obj ref its) (Z.KeyDownKey _ key) = ContT $ \fire ->
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> do
                obj <- Z.doReadIORef ref
                let v = obj ^. its.Z.model.field @"value"
                    v' = J.strip v
                if J.null v'
                    then
                        fire $ pickOnly TodoDestroy
                    else do
                        Z.doModifyIORef' ref (\s -> s
                            & its.Z.model.field @"editing" .~ False
                            & its.Z.model.field @"value" .~ v')
                        Z.dirty this

            "Escape" -> do
                Z.blurRef gid this -- The onBlur handler will trim the value

            _ -> pure ()

todo ::
    ( Z.MonadReactor m
    , Z.MonadJS m
    , Z.MonadHTMLElement m
    ) => Z.Prototype m v TodoInfo (Which '[TodoDestroy])
todo =
    let p = todoView `Z.alsoPrototype` todoInput
    in p & Z.modifyDisplay fdisp
        & Z.modifyInitializer (`Z.handledBy'` (Z.obviousHandler $ hdlStartEdit (Z.GadgetId "input")))
  where
    fdisp disp s =
        let s' = s ^. Z.model
        in Z.bh "div"
            [ ("className", JE.classNames
                [ ("completed", completed s')
                , ("editing", editing s')])
            ]
            (disp s)

    hdlStartEdit ::
        ( Z.MonadReactor m
        , Z.MonadHTMLElement m)
        => Z.GadgetId -> Z.SceneHandler m v TodoInfo TodoStartEdit (Which '[])
    hdlStartEdit i this@(Z.Obj ref its) _ = Z.terminate' $ lift $ do
        void $ runMaybeT $ do
            obj <- lift $ Z.doReadIORef ref
            -- don't allow editing of completed todos
            let b = obj ^. its.Z.model.field @"completed"
            guard (not b)
            lift $ Z.doModifyIORef' ref (\s -> s
                & its.Z.model.field @"editing" .~ True
                )
            lift $ Z.focusRef i this
