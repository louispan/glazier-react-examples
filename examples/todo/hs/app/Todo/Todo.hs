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

import Control.Lens
import Control.Lens.Misc
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified GHC.Generics as G
import Glazier.React
import qualified Glazier.React.Widgets.Input as W
import qualified JavaScript.Extras as JE

data Todo = Todo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    } deriving G.Generic

makeLenses_ ''Todo

data TodoDestroy = TodoDestroy
data TodoStartEdit = TodoStartEdit

todoToggleComplete :: (MkId m, AsReactor cmd) => m (Widget cmd p Todo ())
todoToggleComplete = do
    eid <- mkElementalId "toggle"
    pure $ W.checkboxInput eid
        & _window %~ modifySurfaceProperties (`DL.snoc` ("className", "toggle"))
        & enlargeModel _completed

todoDestroy :: (MkId m, AsReactor cmd) => m (Widget cmd p s TodoDestroy)
todoDestroy = do
    eid <- mkElementalId "destroy"
    pure $ Widget
        { window = lf' eid "button"
                [ ("key", "destroy")
                , ("className", "destroy")]
        , gadget = constTrigger eid _always "onClick" TodoDestroy
        }

todoLabel :: (MkId m, AsReactor cmd) => m (Widget cmd p Todo TodoStartEdit)
todoLabel = do
    eid <- mkElementalId "label"
    pure $ Widget
        { window = do
            str <- view (_model._value)
            bh' eid "label" [("key", "label")] $
                txt str
        , gadget = constTrigger eid _always "onDoubleClick" TodoStartEdit
        }

-- todoView :: (MkId m, AsReactor cmd)
--     => m (Widget cmd p Todo (Which '[(), TodoDestroy, TodoStartEdit]))
-- todoView = do
--     x <- todoToggleComplete
--     y <- todoDestroy
--     z <- todoLabel
--     let wid = (pickOnly <$> x)
--             `also` (pickOnly <$> y)
--             `also` (pickOnly <$> z)
--     pure wid
--     -- pure $ wid & _window %~ \win -> bh "div"
--     --             [ ("key", "view")
--     --             , ("className", "view")]
--     --             win

-- todoInput ::
--     ( MonadReactor m
--     , MonadJS m
--     , MonadHTMLElement m
--     )
--     => Prototype p Todo m TodoDestroy
-- todoInput = (W.textInput eid)
--     & magnifyPrototype _value
--     & _initializer %~ fi
--     & _display %~ fd
--   where
--     eid = GadgetId "input"
--     fd disp s = modifySurfaceProperties
--         (`DL.snoc` ("className", "edit")) (disp s)
--     fi ini = ini
--         ^*> (trigger' eid "onFocus" () >>= hdlFocus)
--         ^*> (trigger' eid "onBlur" () >>= hdlBlur)
--         ^*> (trigger eid "onKeyDown" (runMaybeT . fireKeyDownKey) id
--             >>= maybe mempty hdlKeyDown)

--     hdlFocus :: (MonadReactor m)
--         => a -> MethodT (Scene p m Todo) m ()
--     hdlFocus _ = readrT' $ \this@Obj{..} -> lift $ do
--         doModifyIORef' self (my._model._editing .~ True)
--         dirty this

--     hdlBlur :: (MonadReactor m)
--         => a -> MethodT (Scene p m Todo) m ()
--     hdlBlur _ = readrT' $ \this@Obj{..} -> lift $ do
--         doModifyIORef' self $
--             my._model._editing .~ False
--             >>> my._model._value %~ J.strip
--         dirty this

--     hdlKeyDown ::
--         ( MonadReactor m
--         , MonadHTMLElement m
--         )
--         => KeyDownKey -> MethodT (Scene p m Todo) m TodoDestroy
--     hdlKeyDown (KeyDownKey _ key) = methodT' $ \this@Obj{..} fire ->
--         case key of
--             -- NB. Enter and Escape doesn't generate a onChange event
--             -- So there is no adverse interation with W.input onChange
--             -- updating the value under our feet.
--             "Enter" -> do
--                 me <- doReadIORef self
--                 let v = me ^. my._model._value
--                     v' = J.strip v
--                 if J.null v'
--                     then
--                         fire TodoDestroy
--                     else do
--                         doModifyIORef' self $
--                             my._model._editing .~ False
--                             >>> my._model._value .~ v'
--                         dirty this

--             "Escape" -> do
--                 blurRef eid this -- The onBlur handler will trim the value

--             _ -> pure ()

-- todo ::
--     ( MonadReactor m
--     , MonadJS m
--     , MonadHTMLElement m
--     ) => Prototype p Todo m (Which '[TodoDestroy])
-- todo =
--     let p = todoView `also` (pickOnly <$> todoInput)
--     in p & (_display %~ fd)
--         & (_initializer %~ fi)
--   where
--     fd disp s =
--         let s' = s ^. _model
--         in bh "div"
--             [ ("className", JE.classNames
--                 [ ("completed", completed s')
--                 , ("editing", editing s')])
--             ]
--             (disp s)

--     fi ini = ini >>= (injectedK hdlStartEdit')
--     hdlStartEdit' a = hdlStartEdit (GadgetId "input") (obvious a) ^*> zilch

--     hdlStartEdit ::
--         ( MonadReactor m
--         , MonadHTMLElement m)
--         => GadgetId -> TodoStartEdit -> MethodT (Scene p m Todo) m ()
--     hdlStartEdit i _ = readrT' $ \this@Obj{..} -> lift $ do
--         void $ runMaybeT $ do
--             me <- lift $ doReadIORef self
--             -- don't allow editing of completed todos
--             let b = me ^. my._model._completed
--             guard (not b)
--             lift $ doModifyIORef' self $ my._model._editing .~ True
--             lift $ focusRef i this
