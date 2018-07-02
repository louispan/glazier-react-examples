{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Todo.App where

import Control.Lens
import Control.Lens.Misc
import Control.Monad
import Control.Monad.Trans.Class
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Semigroup.Applicative
import Data.Tagged
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Action.KeyDownKey
import Glazier.React.Effect.HTMLElement
import Glazier.React.Effect.JavaScript
import qualified Glazier.React.Widgets.Collection.Dynamic as W
import qualified Glazier.React.Widgets.Input as W
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD
import qualified Todo.Footer as TD
import qualified Todo.Todo as TD

-- | Just use map order
todoSorter :: Applicative m => srt -> a -> a -> m Ordering
todoSorter _ _ _ = pure LT

todoFilterer :: TD.Filter -> TD.Todo -> ReadIORef Bool
todoFilterer ftr td = do
    -- scn <- doReadIORef $ sceneRef sbj
    pure $ case ftr of
        TD.All -> True
        TD.Active -> not $ TD.completed $ td
        TD.Completed -> TD.completed $ td

data App f = App
    { newTodo :: J.JSString
    , todos :: TD.TodoCollection f
    } deriving (G.Generic)

makeLenses_ ''App

type OnNewTodo = Tagged "OnNewTodo"

newTodoInput :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => ReactId -> Widget cmd p J.JSString (OnNewTodo J.JSString)
newTodoInput ri =
    let wid = finish . void . overWindow fw $ W.textInput ri
    in wid `also` lift gad
  where
    fw = (modifyMarkup (overSurfaceProperties
        (<> [("className", "new-todo"), ("placeholder", "What needs to be done?")])))
    gad = (finish hdlMounted)
        `also` (trigger ri "onKeyDown" fireKeyDownKey
            >>= hdlKeyDown)

    hdlMounted ::
        ( AsReactor cmd
        , AsHTMLElement cmd
        )
        => Gadget cmd p J.JSString ()
    hdlMounted = onMounted $ do
        j <- getElementalRef ri
        exec $ Focus j

    hdlKeyDown :: (AsReactor cmd) => KeyDownKey -> Gadget cmd p J.JSString (OnNewTodo J.JSString)
    hdlKeyDown (KeyDownKey _ key) =
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> tickModelThen $ do
                v <- get
                let v' = J.strip v
                id .= J.empty
                pure $ if J.null v'
                    then finish $ pure ()
                    else pure $ Tagged @"OnNewTodo" v'

            "Escape" -> finish $ tickModel $ id .= J.empty

            _ -> finish $ pure ()

appToggleCompleteAll :: (AsReactor cmd)
    => ReactId -> Widget cmd p (TD.TodoCollection Subject) r
appToggleCompleteAll ri =
    let win = do
            scn <- ask
            props <- lift $ mkProps scn
            lf' ri "input" (DL.fromList props)
        gad = -- (finish $ onElementalRef ri) `also`
            (finish $ hdlChange)
    in (display win) `also` (lift gad)
  where
    mkProps :: Scene (TD.TodoCollection Subject) -> ReadIORef [JE.Property]
    mkProps scn = traverse sequenceA
        [ ("key", pure . JE.toJSR $ ri)
        , ("className", pure "toggle-all")
        , ("type", pure $ "checkbox")
        , ("checked", JE.toJSR <$> (hasActiveTodos (scn ^. _model.W._visibleList)))
        ]

    hasActiveTodos ::  [Subject TD.Todo] -> ReadIORef Bool
    hasActiveTodos = fmap getAny . getAp . foldMap go
      where
        go sbj = Ap $ do
            scn <- doReadIORef $ sceneRef sbj
            pure $ Any $ scn ^. _model.TD._completed

    hdlChange :: (AsReactor cmd) => Gadget cmd p (TD.TodoCollection Subject) ()
    hdlChange = do
        trigger_ ri "onChange" ()
        tickModelThen $ do
            s <- get
            a <- lift $ hasActiveTodos (s ^. W._visibleList)
            pure $ getAls $ foldMap (\sbj -> Als $ lift $ gadgetWith sbj
                    (tickModel $ TD._completed .= not a))
                (view W._visibleList s) -- Only modify visible!

app_ :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => JE.JSRep -> Widget cmd p (App Subject) (OnNewTodo J.JSString)
app_ j = do
    todosRi <- mkReactId "todo-list"
    let newTodo' = magnifyWidget _newTodo $ mkReactId "new-todo" >>= newTodoInput
        appToggleCompleteAll' = magnifyWidget _todos $ mkReactId "complete-all" >>= appToggleCompleteAll
        todoFooter' = magnifyWidget _todos $ mkReactId "todo-footer" >>= (TD.todoFooter j)
        todosWindow = modifyMarkup (overSurfaceProperties
            (<> [("className", "todo-list")]))
            (W.dynamicCollectionWindow todosRi)
        wid = withWindow newTodo' $ \newTodoWin ->
            withWindow appToggleCompleteAll' $ \appToggleCompleteAllWin ->
            withWindow todoFooter' $ \todoFooterWin ->
                let win = do
                        s <- ask
                        bh "div" [("key", "app")] $ do
                            bh "header" [("key", "header"), ("className", "header")] $ do
                                bh "h1" [("key", "heading")] (txt "todos")
                                newTodoWin

                            -- only render if there are todos
                            let ts = s ^. _model._todos.W._rawCollection
                            if M.null ts
                                then pure ()
                                else bh "section" [ ("key", "main")
                                                        , ("className", "main")
                                                        ] $ do
                                    -- Complete all checkbox
                                    appToggleCompleteAllWin

                                    -- Render the list of todos
                                    magnifiedScene _todos todosWindow

                                    -- Render the footer
                                    todoFooterWin
                in display win
        gad = magnifiedEntity _todos $ finish $ onTicked $
            tickModel $ W.updateVisibleList todoFilterer todoSorter
    wid `also` (lift gad)

app :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => JE.JSRep -> Widget cmd p (App Subject) r
app j = app_ j
    >>= (lift . insertTodo')

todoToggleCompleted :: (AsReactor cmd)
    => TD.OnTodoToggleComplete W.UKey -> Gadget cmd p (App Subject) ()
todoToggleCompleted (untag @"OnTodoToggleComplete" -> _) = -- rerender
    tickModel (pure ())

destroyTodo :: (AsReactor cmd)
    => TD.OnTodoDestroy W.UKey -> Gadget cmd p (App Subject) ()
destroyTodo (untag @"OnTodoDestroy" -> k) = do
    tickModelThen $ zoom _todos $ W.deleteDynamicCollectionItem k

type OnTodoTicked = Tagged "OnTodoTicked"

tickedTodo :: (AsReactor cmd)
    => OnTodoTicked W.UKey -> Gadget cmd p (App Subject) ()
tickedTodo (untag @"OnTodoTicked" -> _) =
    magnifiedEntity _todos $ tickModel $ pure () -- trigger onTicked for App

insertTodo :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => OnNewTodo J.JSString -> Gadget cmd p (App Subject) (Which '[TD.OnTodoToggleComplete W.UKey, TD.OnTodoDestroy W.UKey, OnTodoTicked W.UKey])
insertTodo (untag @"OnNewTodo" -> n) = do
    s <- getModel
    let mk = view (_todos.W._rawCollection.to M.lookupMax) s
        k' = case mk of
            Just (k, _) -> W.largerUKey k
            Nothing -> W.zeroUKey
    withMkSubject (go k' <$> todo') (TD.Todo n False False) $ \sbj -> do
        tickModelThen $ zoom _todos $ W.insertDynamicCollectionItem k' sbj
  where
    todo' = TD.todo
        & chooseWith also $ (onTicked (pure $ pickOnly $ Tagged @"OnTodoTicked" ()))
    go :: W.UKey -> Which '[TD.OnTodoToggleComplete (), TD.OnTodoDestroy (), OnTodoTicked ()] -> Which '[TD.OnTodoToggleComplete W.UKey, TD.OnTodoDestroy W.UKey, OnTodoTicked W.UKey]
    go k y = afmap (CaseFunc1 @C0 @Functor @C0 (fmap (const k))) y

insertTodo' :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => OnNewTodo J.JSString -> Gadget cmd p (App Subject) r
insertTodo' a = insertTodo a
    >>= (injectedK $ totally . finish . todoToggleCompleted . obvious)
    >>= (injectedK $ totally . finish . destroyTodo . obvious)
    >>= (injectedK $ totally . finish . tickedTodo . obvious)
    & fmap impossible
