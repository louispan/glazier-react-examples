{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Todo
    ( Todo(..)
    , _value
    , _completed
    , _editing
    , TodoDestroy
    , mkTodo
    ) where

import Control.Lens
import Control.Lens.Misc
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import Data.Maybe
import Data.Semigroup
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Action.KeyDownKey
import Glazier.React.Effect.HTMLElement
import Glazier.React.Effect.JavaScript
import qualified Glazier.React.Widgets.Input as W
import qualified JavaScript.Extras as JE

data Todo = Todo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    } deriving (Show, Eq, Ord, G.Generic)

makeLenses_ ''Todo

data TodoDestroy = TodoDestroy
data TodoComplete = TodoComplete
data TodoStartEdit = TodoStartEdit

todoToggleComplete :: (AsReactor cmd) => ElementalId -> Widget cmd p Todo TodoComplete
todoToggleComplete eid =
    W.checkboxInput eid
        & _window %~ modifySurfaceProperties (`DL.snoc` ("className", "toggle"))
        & _gadget %~ (fmap $ const TodoComplete)
        & enlargeModel _completed

todoDestroy :: (AsReactor cmd) => ElementalId -> Widget cmd p s TodoDestroy
todoDestroy eid =
    blank
    { window = lf' eid "button"
            [ ("key", "destroy")
            , ("className", "destroy")]
    , gadget = trigger_ eid _always "onClick" TodoDestroy
    }

todoLabel :: (AsReactor cmd) => ElementalId -> Widget cmd p Todo TodoStartEdit
todoLabel eid =
    blank
    { window = do
        str <- view (_model._value)
        bh' eid "label" [("key", "label")] $
            txt str
    , gadget = trigger_ eid _always "onDoubleClick" TodoStartEdit
    }

mkTodoView :: (MkId m, AsReactor cmd) => m (Widget cmd p Todo (Which '[TodoComplete, TodoDestroy, TodoStartEdit]))
mkTodoView = do
    todoToggleComplete' <- todoToggleComplete <$> mkElementalId "toggle"
    todoDestroy' <- todoDestroy <$> mkElementalId "destroy"
    todoLabel' <- todoLabel <$> mkElementalId "label"
    let wid = (pickOnly <$> todoToggleComplete')
            `also` (pickOnly <$> todoDestroy')
            `also` (pickOnly <$> todoLabel')
    pure $ wid & _window %~ \win -> bh "div"
                [ ("key", "view")
                , ("className", "view")]
                win

todoInput :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd) => ElementalId -> Widget cmd p Todo TodoDestroy
todoInput eid =
    W.textInput eid
        & enlargeModel _value
        & _window %~ f
        & _gadget %~ g
  where
    f = modifySurfaceProperties (`DL.snoc` ("className", "edit"))
    g gad = (finish gad)
        <> (finish $ hdlFocus)
        <> (finish $ hdlBlur)
        <> hdlKeyDown

    hdlFocus :: AsReactor cmd => Gadget cmd p Todo ()
    hdlFocus = trigger_ eid _always "onFocus" () *> tickScene (_model._editing .= True)

    hdlBlur :: AsReactor cmd => Gadget cmd p Todo ()
    hdlBlur = do
        trigger_ eid _always "onBlur" ()
        tickScene $ do
            _model._editing .= False
            _model._value %= J.strip

    hdlKeyDown :: (AsReactor cmd, AsHTMLElement cmd) => Gadget cmd p Todo TodoDestroy
    hdlKeyDown = do
        (KeyDownKey _ key) <- trigger' eid _always "onKeyDown" fireKeyDownKey
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> tickSceneThen $ do
                v <- use (_model._value)
                let v' = J.strip v
                if J.null v'
                    then
                        pure $ pure TodoDestroy
                    else do
                        _model._editing .= False
                        _model._value .= v'
                        pure $ finish $ pure ()

            "Escape" -> finish $ blurElement eid -- The onBlur handler will trim the value

            _ -> finish $ pure ()

mkTodo :: (MkId m, AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => m (Widget cmd p Todo (Which '[TodoComplete, TodoDestroy]))
mkTodo = do
    todoView' <- mkTodoView
    eid <- mkElementalId "input"
    let todoInput' = todoInput eid
    let w = todoView' `also` (pickOnly <$> todoInput')
    pure $ w
        & (_window %~ f)
        & (_gadget %~ g eid)
  where
    f win = do
        s <- view _model
        bh "div"
            [ ("className", JE.classNames
                [ ("completed", completed s)
                , ("editing", editing s)])
            ]
            win
    g eid gad = gad >>= hdlStartEdit' eid
    hdlStartEdit' eid = injectedK $ finished . hdlStartEdit eid . obvious

    hdlStartEdit :: (AsHTMLElement cmd, AsReactor cmd)
        => ElementalId -> TodoStartEdit -> Gadget cmd p Todo ()
    hdlStartEdit eid _ = tickSceneThen . fmap (fromMaybe (pure ())) . runMaybeT $ do
            -- don't allow editing of completed todos
            b <- use (_model._completed)
            guard (not b)
            _model._editing .= True
            pure $ focusElement eid
