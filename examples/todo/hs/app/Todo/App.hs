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
import Data.Maybe
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
    -- scn <- doReadIORef $ modelRef obj
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
    => ReactId -> Widget cmd p J.JSString (OnNewTodo (ReactId, J.JSString))
newTodoInput k =
    let wid = finish . void . overWindow fw $ W.textInput k
    in wid `also` lift gad
  where
    fw = (modifyMarkup (overSurfaceProperties
        (<> [("className", "new-todo"), ("placeholder", "What needs to be done?")])))
    gad = (finish hdlMounted)
        `also` (trigger k "onKeyDown" fireKeyDownKey
            >>= hdlKeyDown)

    hdlMounted ::
        ( AsReactor cmd
        , AsHTMLElement cmd
        )
        => Gadget cmd p J.JSString ()
    hdlMounted = onMounted $ do
        j <- getElementalRef k
        exec $ Focus j

    hdlKeyDown :: (AsReactor cmd) => KeyDownKey -> Gadget cmd p J.JSString (OnNewTodo (ReactId, J.JSString))
    hdlKeyDown (KeyDownKey _ key) =
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> mutateThen k $ do
                v <- get
                let v' = J.strip v
                id .= J.empty
                pure $ if J.null v'
                    then do
                        debugIO_ $ putStrLn "empty todo"
                        finish $ pure ()
                    else do
                        debugIO_ $ putStrLn "new todo entered"
                        pure $ Tagged @"OnNewTodo" (k, v')

            "Escape" -> finish $ mutate k $ id .= J.empty

            _ -> finish $ pure ()

appToggleCompleteAll :: (AsReactor cmd)
    => ReactId -> Widget cmd p (TD.TodoCollection Obj) r
appToggleCompleteAll k =
    let win = do
            scn <- ask
            props <- lift $ mkProps scn
            lf' k "input" (DL.fromList props)
        gad = -- (finish $ onElementalRef k) `also`
            (finish $ hdlChange)
    in (display win) `also` (lift gad)
  where
    mkProps :: Model (TD.TodoCollection Obj) -> ReadIORef [JE.Property]
    mkProps scn = traverse sequenceA
        [ ("key", pure $ reactIdKey' k)
        , ("className", pure "toggle-all")
        , ("type", pure $ "checkbox")
        , ("checked", JE.toJSR <$> (hasActiveTodos (scn ^. _model.W._visibleList)))
        ]

    hasActiveTodos :: [Obj TD.Todo] -> ReadIORef Bool
    hasActiveTodos = fmap getAny . getAp . foldMap go
      where
        go obj = Ap $ do
            scn <- doReadIORef $ modelRef obj
            pure $ Any $ scn ^. _model.TD._completed

    hdlChange :: (AsReactor cmd) => Gadget cmd p (TD.TodoCollection Obj) ()
    hdlChange = do
        trigger_ k "onChange" ()
        put "App ToggleAll"
        mutateThen k $ do
            s <- get
            a <- lift $ hasActiveTodos (s ^. W._visibleList)
            pure $ getAls $ foldMap (go a) (view W._visibleList s) -- Only modify visible!
      where
        go a obj = Als $ do
            -- lift from ContT to GadgetT
            lift $ gadgetWith' obj (mutate k $ TD._completed .= not a)

app_ :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => JE.JSRep -> ReactId -> Widget cmd p (App Obj) (OnNewTodo (ReactId, J.JSString))
app_ j todosK = do
    newTodoK <- mkReactId "new-todo"
    let newTodo' = magnifyWidget _newTodo $ newTodoInput newTodoK
        appToggleCompleteAll' = magnifyWidget _todos $ mkReactId "complete-all" >>= appToggleCompleteAll
        todoFooter' = magnifyWidget _todos $ mkReactId "todo-footer" >>= TD.todoFooter j
        todosWindow = modifyMarkup (overSurfaceProperties
            (<> [("className", "todo-list")]))
            (W.dynamicCollectionWindow todosK)
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
                                    magnifiedModel _todos todosWindow

                                    -- Render the footer
                                    todoFooterWin
                in display win
        gad = finish $ do
            -- FIXME: this is being called on every keystroke in the input!
            -- FIXME: don't update visible if k is from new-todo
            put "App Mutated"
            onMutated $ \k ->
                -- ignore updates from the new-todo input box
                if k == newTodoK
                    then pure ()
                    else updateTodos k
    wid `also` (lift gad)

updateTodos :: (AsReactor cmd)
    => ReactId -> Gadget cmd p (App Obj) ()
updateTodos k = magnifiedEntity _todos $
    mutate k $ W.updateVisibleList todoFilterer todoSorter

app :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => JE.JSRep -> Widget cmd p (App Obj) r
app j = do
    todosK <- mkReactId "todo-list"
    app_ j todosK >>= (lift . insertTodo' todosK)

todoToggleCompleted :: (AsReactor cmd)
    => TD.OnTodoToggleComplete (ReactId, W.UKey) -> Gadget cmd p (App Obj) ()
todoToggleCompleted (untag @"OnTodoToggleComplete" -> (k, _)) = updateTodos k

destroyTodo :: (AsReactor cmd)
    => TD.OnTodoDestroy (ReactId, W.UKey) -> Gadget cmd p (App Obj) ()
destroyTodo (untag @"OnTodoDestroy" -> (k, i)) =
    mutate k $ _todos.W._rawCollection.(at i) .= Nothing

type OnTodoMutated = Tagged "OnTodoMutated"

tickedTodo :: (AsReactor cmd)
    => OnTodoMutated (ReactId, W.UKey) -> Gadget cmd p (App Obj) ()
tickedTodo (untag @"OnTodoMutated" -> (k, _)) = updateTodos k

insertTodo :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => ReactId -> OnNewTodo (ReactId, J.JSString) -> Gadget cmd p (App Obj)
        (Which '[TD.OnTodoToggleComplete (ReactId, W.UKey), TD.OnTodoDestroy (ReactId, W.UKey), OnTodoMutated (ReactId, W.UKey)])
insertTodo todosK (untag @"OnNewTodo" -> (_, n)) = do
    s <- getModel
    let mi = view (_todos.W._rawCollection.to lookupMax) s
        i' = case mi of
            Just (i, _) -> W.largerUKey i
            Nothing -> W.zeroUKey
    put "New todo"
    -- use todosK for mutate so app can have a more efficient onMutated
    withMkObj (go i' <$> todo') (TD.Todo n False False) $ \obj ->
        mutate todosK $ _todos.W._rawCollection.(at i') .= Just obj
  where
    lookupMax = listToMaybe . M.toDescList
    todo' = TD.todo
        & chooseWith also $ (onMutated $ \k' -> pure $ pickOnly $ Tagged @"OnTodoMutated" k')
    go :: W.UKey -> Which '[TD.OnTodoToggleComplete ReactId, TD.OnTodoDestroy ReactId, OnTodoMutated ReactId]
        -> Which '[TD.OnTodoToggleComplete (ReactId, W.UKey), TD.OnTodoDestroy (ReactId, W.UKey), OnTodoMutated (ReactId, W.UKey)]
    go i y = afmap (CaseFunc1_ @C0 @Functor @C0 @(ReactId, W.UKey) (fmap (\k' -> (k', i)))) y

insertTodo' :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => ReactId -> OnNewTodo (ReactId, J.JSString) -> Gadget cmd p (App Obj) r
insertTodo' todosK a = insertTodo todosK a
    >>= (injectedK $ totally . finish . todoToggleCompleted . obvious)
    >>= (injectedK $ totally . finish . destroyTodo . obvious)
    >>= (injectedK $ totally . finish . tickedTodo . obvious)
    & fmap impossible
