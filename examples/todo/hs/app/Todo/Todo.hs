{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Todo.Todo
    ( Todo(..)
    , _value
    , _completed
    , _editing
    , OnTodoDestroy
    , OnTodoToggleComplete
    , todo
    ) where

import Control.Lens
import Control.Lens.Misc
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import Data.Tagged
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

type OnTodoDestroy = Tagged "OnTodoDestroy"
type OnTodoToggleComplete = Tagged "OnTodoToggleComplete"
type OnTodoStartEdit = Tagged "OnTodoStartEdit"

todoToggleComplete :: (AsReactor cmd) => ReactId -> Widget cmd p Todo (OnTodoToggleComplete ReactId)
todoToggleComplete k =
    let wid = (retag @"InputChange" @_ @"OnTodoToggleComplete") <$> overWindow fw (W.checkboxInput k)
    in magnifyWidget _completed wid
  where
    fw = (modifyMarkup (overSurfaceProperties (`DL.snoc` ("className", "toggle"))))

todoDestroy :: (AsReactor cmd) => ReactId -> Widget cmd p s (OnTodoDestroy ReactId)
todoDestroy k =
    let win = lf' k "button"
            [ ("key", "destroy")
            , ("className", "destroy")]
        gad = trigger_ k "onClick" (Tagged @"OnTodoDestroy" k)
    in (display win) `also` (lift gad)

todoLabel :: (AsReactor cmd) => ReactId -> Widget cmd p Todo (OnTodoStartEdit ReactId)
todoLabel k =
    let win = do
            str <- view (_model._value)
            bh' k "label" [("key", "label")] $
                txt str
        gad = trigger_ k "onDoubleClick" (Tagged @"OnTodoStartEdit" k)
    in (display win) `also` (lift gad)

todoView :: (AsReactor cmd) => Widget cmd p Todo (Which '[OnTodoToggleComplete ReactId, OnTodoDestroy ReactId, OnTodoStartEdit ReactId])
todoView =
    let todoToggleComplete' = pickOnly <$> (mkReactId "toggle" >>= todoToggleComplete)
        todoDestroy' = pickOnly <$> (mkReactId "destroy" >>= todoDestroy)
        todoLabel' = pickOnly <$> (mkReactId "label" >>= todoLabel)
        wid = withWindow' todoToggleComplete' $ \todoToggleCompleteWin' ->
            withWindow' todoDestroy' $ \todoDestroyWin' ->
            withWindow' todoLabel' $ \todoLabelWin' ->
                totally $ display $ bh "div"
                    [ ("key", "view")
                    , ("className", "view")] $
                        todoToggleCompleteWin'
                        *> todoLabelWin'
                        *> todoDestroyWin'
    in wid

todoInput :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd) => ReactId -> Widget cmd p Todo (OnTodoDestroy ReactId)
todoInput k =
    let wid = overWindow fw . magnifyWidget _value $ W.textInput k
        wid' = finish (void wid) `also` lift gad
    in wid'
  where
    fw = (modifyMarkup (overSurfaceProperties (`DL.snoc` ("className", "edit"))))
    gad = (finish hdlBlur)
        `also` hdlKeyDown

    hdlBlur :: (AsReactor cmd) => Gadget cmd p Todo ()
    hdlBlur = do
        trigger_ k "onBlur" ()
        mutate k $ do
            _editing .= False
            _value %= J.strip

    hdlKeyDown :: (AsReactor cmd, AsHTMLElement cmd) => Gadget cmd p Todo (OnTodoDestroy ReactId)
    hdlKeyDown = do
        (KeyDownKey _ key) <- trigger k "onKeyDown" fireKeyDownKey
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> mutateThen k $ do
                v <- use _value
                let v' = J.strip v
                if J.null v'
                    then
                        pure $ pure $ Tagged @"OnTodoDestroy" k
                    else do
                        _editing .= False
                        _value .= v'
                        pure $ finish $ pure ()

            "Escape" -> finish $ do
                j <- getElementalRef k
                exec $ Blur j -- The onBlur handler will trim the value

            _ -> finish $ pure ()

todo :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => Widget cmd p Todo (Which '[OnTodoToggleComplete ReactId, OnTodoDestroy ReactId])
todo = do
    k <- mkReactId "input"
    let todoInput' = pickOnly <$> todoInput k
        wid = overWindow2' (*>) todoView todoInput'
    overWindow fw wid >>= (injectedK $ lift . totally . finish . hdlStartEdit k . obvious)

  where
    fw win = do
        s <- view _model
        bh "li"
            [ ("className", JE.classNames
                [ ("completed", completed s)
                , ("editing", editing s)])
            ]
            win

    hdlStartEdit :: (AsHTMLElement cmd, AsJavascript cmd, AsReactor cmd)
        => ReactId -> OnTodoStartEdit ReactId -> Gadget cmd p Todo ()
    hdlStartEdit k (untag @"OnTodoStartEdit" -> _) = do
        mutate k $ _editing .= True
        j <- getElementalRef k
        onNextRendered $ do
            exec $ Focus j
            (`evalMaybeT` ()) $ do
                v <- maybeGetProperty "value" j
                let i = J.length v
                exec' $ SetProperty ("selectionStart", JE.toJSR (0 :: Int)) j
                exec' $ SetProperty ("selectionEnd", JE.toJSR i) j
