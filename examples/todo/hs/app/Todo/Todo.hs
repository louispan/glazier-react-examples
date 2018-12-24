{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified Data.Aeson as A
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
import JavaScript.Extras.Aeson.Instances ()

data Todo = Todo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    } deriving (Show, Eq, Ord, G.Generic)

makeLenses_ ''Todo

instance A.ToJSON Todo where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON Todo

type OnTodoDestroy = Tagged "OnTodoDestroy"
type OnTodoToggleComplete = Tagged "OnTodoToggleComplete"
type OnTodoStartEdit = Tagged "OnTodoStartEdit"

instance FromModel Todo where
    mkEncoding Todo {..} = pure $ A.pairs
        ( "value" A..= value
        <> "completed" A..= completed
        <> "editing" A..= editing
        )

instance ToModel Todo where
    parseModelJSON = fmap pure . go
      where
        go = A.withObject "Todo" $ \v -> Todo
            <$> v A..: "value"
            <*> v A..: "completed"
            <*> v A..: "editing"

todoToggleComplete :: (AsReactor c) => ReactId -> Widget c o Todo (OnTodoToggleComplete ReactId)
todoToggleComplete k =
    let wid = (retag @"InputChange" @_ @"OnTodoToggleComplete") <$> overWindow fw (W.checkboxInput k)
    in magnifyWidget _completed wid
  where
    fw = (modifyMarkup (overSurfaceProperties (`DL.snoc` ("className", "toggle"))))

todoDestroy :: (AsReactor c) => ReactId -> Widget c o s (OnTodoDestroy ReactId)
todoDestroy k =
    let win = lf' k "button"
            [ ("key", "destroy")
            , ("className", "destroy")]
        gad = trigger_ k "onClick" (Tagged @"OnTodoDestroy" k)
    in (display win) `also` (lift gad)


todoLabel :: (AsReactor c) => ReactId -> Widget c o Todo (OnTodoStartEdit ReactId)
todoLabel k =
    let win = do
            str <- view (_model._value)
            bh' k "label" [("key", "label")] $
                txt str
        gad = trigger_ k "onDoubleClick" (Tagged @"OnTodoStartEdit" k)
    in (display win) `also` (lift gad)

todoView :: (AsReactor c) => Widget c o Todo (Which '[OnTodoToggleComplete ReactId, OnTodoDestroy ReactId, OnTodoStartEdit ReactId])
todoView =
    let todoToggleComplete' = pickOnly <$> (mkReactId "toggle" >>= todoToggleComplete)
        todoDestroy' = pickOnly <$> (mkReactId "destroy" >>= todoDestroy)
        todoLabel' = pickOnly <$> (mkReactId "label" >>= todoLabel)
        wid = withWindow' todoToggleComplete' $ \todoToggleCompleteWin' ->
            withWindow' todoDestroy' $ \todoDestroyWin' ->
            withWindow' todoLabel' $ \todoLabelWin' ->
                display' $ bh "div"
                    [ ("key", "view")
                    , ("className", "view")] $
                        todoToggleCompleteWin'
                        *> todoLabelWin'
                        *> todoDestroyWin'
    in wid

todoInput :: (AsReactor c, AsJavascript c, AsHTMLElement c) => ReactId -> Widget c o Todo (OnTodoDestroy ReactId)
todoInput k =
    let wid = overWindow fw . magnifyWidget _value $ W.textInput k
        wid' = finish (void wid) `also` lift gad
    in wid'
  where
    fw = (modifyMarkup (overSurfaceProperties (`DL.snoc` ("className", "edit"))))
    gad = (finish hdlBlur)
        `also` hdlKeyDown

    hdlBlur :: (AsReactor c) => Gadget c o Todo ()
    hdlBlur = do
        trigger_ k "onBlur" ()
        mutate k $ do
            _editing .= False
            _value %= J.strip

    hdlKeyDown :: (AsReactor c, AsHTMLElement c) => Gadget c o Todo (OnTodoDestroy ReactId)
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
                blur j -- The onBlur handler will trim the value

            _ -> finish $ pure ()

todo :: (AsReactor c, AsJavascript c, AsHTMLElement c)
    => Widget c o Todo (Which '[OnTodoToggleComplete ReactId, OnTodoDestroy ReactId])
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

    hdlStartEdit :: (AsHTMLElement c, AsJavascript c, AsReactor c)
        => ReactId -> OnTodoStartEdit ReactId -> Gadget c o Todo ()
    hdlStartEdit k (untag @"OnTodoStartEdit" -> _) = do
        mutate k $ _editing .= True
        j <- getElementalRef k
        onNextRendered $ do
            focus j
            (`evalMaybeT` ()) $ do
                v <- MaybeT $ JE.fromJSR <$> getProperty "value" j
                let i = J.length v
                setProperty ("selectionStart", JE.toJSR (0 :: Int)) j
                setProperty ("selectionEnd", JE.toJSR i) j
