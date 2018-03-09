{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Todo where

import Control.Arrow
import Control.Lens
import Control.Lens.Misc
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import Esoteric
import qualified GHC.Generics as G
import Glazier.React.Framework
import qualified Glazier.React.Widget.Input as W
import qualified JavaScript.Extras as JE

data Todo = Todo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    } deriving G.Generic

makeLenses_ ''Todo

data TodoDestroy = TodoDestroy
data TodoStartEdit = TodoStartEdit

todoToggleComplete :: (MonadReactor m) => Prototype p Todo m ()
todoToggleComplete = W.checkboxInput (GadgetId "toggle")
    & _display %~ fd
    & magnifyPrototype _completed
 where
    fd disp s = modifySurfaceProperties
        (`DL.snoc` ("className", "toggle")) (disp s)

todoDestroy :: (MonadReactor m) => Prototype p s m TodoDestroy
todoDestroy = mempty
    { display = \s -> lf' gid s "button"
            [ ("key", "destroy")
            , ("className", "destroy")]
    , initializer = trigger' gid "onClick" TodoDestroy
    }
  where
    gid = GadgetId "destroy"


todoLabel :: (MonadReactor m) => Prototype p Todo m TodoStartEdit
todoLabel = mempty
        { display = disp
        , initializer = trigger' gid "onDoubleClick" TodoStartEdit
        }
  where
    gid = GadgetId "label"
    disp :: (MonadReactor m) => FrameDisplay Todo m ()
    disp s = bh' gid s "label" [("key", "label")] $
                txt (s ^. _model._value)

todoView :: (MonadReactor m) => Prototype p Todo m (Which '[TodoDestroy, TodoStartEdit])
todoView =
    let p = todoToggleComplete
            ^*> (pickOnly <$> todoDestroy)
            `also` (pickOnly <$> todoLabel)
        disp = display p
    in p { display = \s -> bh "div"
                [ ("key", "view")
                , ("className", "view")]
                (disp s) }

todoInput ::
    ( MonadReactor m
    , MonadJS m
    , MonadHTMLElement m
    )
    => Prototype p Todo m TodoDestroy
todoInput = (W.textInput gid)
    & magnifyPrototype _value
    & _initializer %~ fi
    & _display %~ fd
  where
    gid = GadgetId "input"
    fd disp s = modifySurfaceProperties
        (`DL.snoc` ("className", "edit")) (disp s)
    fi ini = ini
        ^*> (trigger' gid "onFocus" () >>= hdlFocus)
        ^*> (trigger' gid "onBlur" () >>= hdlBlur)
        ^*> (trigger gid "onKeyDown" (runMaybeT . fireKeyDownKey) id
            >>= maybe mempty hdlKeyDown)

    hdlFocus :: (MonadReactor m)
        => a -> MethodT (Scene p m Todo) m ()
    hdlFocus _ = readrT' $ \this@Obj{..} -> lift $ do
        doModifyIORef' self (my._model._editing .~ True)
        dirty this

    hdlBlur :: (MonadReactor m)
        => a -> MethodT (Scene p m Todo) m ()
    hdlBlur _ = readrT' $ \this@Obj{..} -> lift $ do
        doModifyIORef' self $
            my._model._editing .~ False
            >>> my._model._value %~ J.strip
        dirty this

    hdlKeyDown ::
        ( MonadReactor m
        , MonadHTMLElement m
        )
        => KeyDownKey -> MethodT (Scene p m Todo) m TodoDestroy
    hdlKeyDown (KeyDownKey _ key) = methodT' $ \this@Obj{..} fire ->
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> do
                me <- doReadIORef self
                let v = me ^. my._model._value
                    v' = J.strip v
                if J.null v'
                    then
                        fire TodoDestroy
                    else do
                        doModifyIORef' self $
                            my._model._editing .~ False
                            >>> my._model._value .~ v'
                        dirty this

            "Escape" -> do
                blurRef gid this -- The onBlur handler will trim the value

            _ -> pure ()

todo ::
    ( MonadReactor m
    , MonadJS m
    , MonadHTMLElement m
    ) => Prototype p Todo m (Which '[TodoDestroy])
todo =
    let p = todoView `also` (pickOnly <$> todoInput)
    in p & (_display %~ fd)
        & (_initializer %~ fi)
  where
    fd disp s =
        let s' = s ^. _model
        in bh "div"
            [ ("className", JE.classNames
                [ ("completed", completed s')
                , ("editing", editing s')])
            ]
            (disp s)

    fi ini = ini >>= (injectedK hdlStartEdit')
    hdlStartEdit' a = hdlStartEdit (GadgetId "input") (obvious a) ^*> zilch

    hdlStartEdit ::
        ( MonadReactor m
        , MonadHTMLElement m)
        => GadgetId -> TodoStartEdit -> MethodT (Scene p m Todo) m ()
    hdlStartEdit i _ = readrT' $ \this@Obj{..} -> lift $ do
        void $ runMaybeT $ do
            me <- lift $ doReadIORef self
            -- don't allow editing of completed todos
            let b = me ^. my._model._completed
            guard (not b)
            lift $ doModifyIORef' self $ my._model._editing .~ True
            lift $ focusRef i this
