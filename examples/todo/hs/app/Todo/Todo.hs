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
    , todo
    ) where

import Control.Lens
import Control.Lens.Misc
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import Data.Maybe
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

todoToggleComplete :: (AsReactor cmd) => ReactId -> Widget cmd p Todo TodoComplete
todoToggleComplete ri =
    let wid = const TodoComplete <$> overWindow fw (W.checkboxInput ri)
    in magnifyWidget _completed wid
  where
    fw = (*> modify' (overSurfaceProperties (`DL.snoc` ("className", "toggle"))))

todoDestroy :: (AsReactor cmd) => ReactId -> Widget cmd p s TodoDestroy
todoDestroy ri =
    let win = lf' ri "button"
            [ ("key", "destroy")
            , ("className", "destroy")]
        gad = trigger_ ri _always "onClick" TodoDestroy
    in (display win) `also` (lift gad)

todoLabel :: (AsReactor cmd) => ReactId -> Widget cmd p Todo TodoStartEdit
todoLabel ri =
    let win = do
            str <- view (_model._value)
            bh' ri "label" [("key", "label")] $
                txt str
        gad = trigger_ ri _always "onDoubleClick" TodoStartEdit
    in (display win) `also` (lift gad)

todoView :: (AsReactor cmd) => Widget cmd p Todo (Which '[TodoComplete, TodoDestroy, TodoStartEdit])
todoView =
    let todoToggleComplete' = mkReactId "toggle" >>= todoToggleComplete
        todoDestroy' = mkReactId "destroy" >>= todoDestroy
        todoLabel' = mkReactId "label" >>= todoLabel
        wid = (pickOnly <$> todoToggleComplete')
            & chooseWith also $ (pickOnly <$> todoDestroy')
            & chooseWith also $ (pickOnly <$> todoLabel')
        fw = bh "div"
                [ ("key", "view")
                , ("className", "view")]
    in overWindow fw wid

todoInput :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd) => ReactId -> Widget cmd p Todo TodoDestroy
todoInput ri =
    let wid = overWindow fw . magnifyWidget _value $ W.textInput ri
        wid' = finish (void wid) `also` lift gad
    in wid'
  where
    fw = (*> modify' (overSurfaceProperties (`DL.snoc` ("className", "edit"))))
    gad = (finish $ hdlFocus)
        `also` (finish $ hdlBlur)
        `also` hdlKeyDown

    hdlFocus :: AsReactor cmd => Gadget cmd p Todo ()
    hdlFocus = trigger_ ri _always "onFocus" () *> tickScene (_model._editing .= True)

    hdlBlur :: AsReactor cmd => Gadget cmd p Todo ()
    hdlBlur = do
        trigger_ ri _always "onBlur" ()
        tickScene $ do
            _model._editing .= False
            _model._value %= J.strip

    hdlKeyDown :: (AsReactor cmd, AsHTMLElement cmd) => Gadget cmd p Todo TodoDestroy
    hdlKeyDown = do
        (KeyDownKey _ key) <- trigger' ri _always "onKeyDown" fireKeyDownKey
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

            "Escape" -> finish $ blurElement ri -- The onBlur handler will trim the value

            _ -> finish $ pure ()

todo :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => Widget cmd p Todo (Which '[TodoComplete, TodoDestroy])
todo = do
    ri <- mkReactId "input"
    let todoInput' = todoInput ri
        wid = todoView & chooseWith also $ (pickOnly <$> todoInput')
    overWindow fw wid >>= hdlStartEdit' ri
  where
    fw win = do
        s <- view _model
        bh "div"
            [ ("className", JE.classNames
                [ ("completed", completed s)
                , ("editing", editing s)])
            ]
            win

    hdlStartEdit' ri = injectedK $  lift . finished . hdlStartEdit ri . obvious

    hdlStartEdit :: (AsHTMLElement cmd, AsReactor cmd)
        => ReactId -> TodoStartEdit -> Gadget cmd p Todo ()
    hdlStartEdit ri _ = tickSceneThen . fmap (fromMaybe (pure ())) . runMaybeT $ do
            -- don't allow editing of completed todos
            b <- use (_model._completed)
            guard (not b)
            _model._editing .= True
            pure $ focusElement ri
