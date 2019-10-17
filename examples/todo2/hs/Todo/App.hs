{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Todo.App where

import Control.Lens
import Control.Lens.Misc
import Control.Monad
import Data.Functor.Compose
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Applicative as A
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Semigroup.Applicative
import Data.Tagged
import Data.UKey
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

todoFilterer :: TD.Filter -> Obj TD.Todo -> Benign IO Bool
todoFilterer ftr td = do
    td' <- meta <$> (benignReadIORef $ sceneRef td)
    pure $ case ftr of
        TD.All -> True
        TD.Active -> not $ TD.completed $ td'
        TD.Completed -> TD.completed $ td'

data App = App
    { newTodo :: J.JSString
    , todos :: TD.TodoCollection
    } deriving (G.Generic)

makeLenses_ ''App

instance MonadIO m => A.AToJSON (Benign m) App
instance (Applicative m, AsJavascript c, AsHTMLElement c, MonadReactor c m) => A.AFromJSON (ExceptT (Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)]) m) App where
    aparseJSON = A.withObject "App" $ \v -> fmap initialize (aparse' v)
      where
        aparse' v = getCompose $ (,)
            <$> (Compose $ A.aparseField v "newTodo")
            <*> (Compose $ A.aparseField v "todos")
        initialize m = do
            (v, xs) <- m
            xs' <- initialize' xs
            pure $ App v xs'
        initialize' :: (AsJavascript c, AsHTMLElement c, MonadReactor c m) => W.DynamicCollection TD.Filter () UKey TD.Todo -> (ExceptT (Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)]) m) TD.TodoCollection
        initialize' W.DynamicCollection {..} = do
            xs <- M.traverseWithKey (\k -> ExceptT . doInsertTodo k) rawCollection
            let xs' = W.DynamicCollection filterCriteria sortCriteria [] xs
            lift $ evalBenignIO $ execStateT (W.updateVisibleList todoFilterer todoSorter) xs'

type OnNewTodo = Tagged "OnNewTodo"

-- | Use a different ReactId when firing OnNewTodo so that it is easier
-- to differentiate between onChanged typing events and new todo event.
todoInput :: (AsReactor c, AsJavascript c, AsHTMLElement c)
    => ReactId -> ReactId -> Widget c o J.JSString (OnNewTodo (ReactId, J.JSString))
todoInput onNewTodoK k =
    let wid = finish . void . overWindow fw $ W.textInput k
    in wid `also` lift gad
  where
    fw = (modifyMarkup (overSurfaceProperties
        (<> [("className", "new-todo"), ("placeholder", "What needs to be done?")])))
    gad = (finish hdlMounted)
        `also` (trigger k "onKeyDown" fireKeyDownKey
            >>= hdlKeyDown)

    hdlMounted ::
        ( AsReactor c
        , AsHTMLElement c
        )
        => Gadget c o J.JSString ()
    hdlMounted = onMounted $ do
        j <- getReactRef k
        exec $ Focus j

    hdlKeyDown :: (AsReactor c) => KeyDownKey -> Gadget c o J.JSString (OnNewTodo (ReactId, J.JSString))
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
                        pure $ Tagged @"OnNewTodo" (onNewTodoK, v')

            "Escape" -> finish $ mutate k $ id .= J.empty

            _ -> finish $ pure ()

appToggleCompleteAll :: (AsReactor c)
    => ReactId -> Widget c o TD.TodoCollection r
appToggleCompleteAll k =
    let win = do
            mdl <- ask
            props <- lift $ mkProps mdl
            lf' k "input" (DL.fromList props)
        gad = -- (finish $ onElementalRef k) `also`
            (finish $ hdlChange)
    in (display win) `also` (lift gad)
  where
    mkProps :: Meta TD.TodoCollection -> Benign IO [JE.Property]
    mkProps mdl = traverse sequenceA
        [ ("key", pure $ reactIdKey' k)
        , ("className", pure "toggle-all")
        , ("type", pure $ "checkbox")
        , ("checked", JE.toJSR <$> (hasActiveTodos (mdl ^. _meta.W._visibleList)))
        ]

    hasActiveTodos :: [Obj TD.Todo] -> Benign IO Bool
    hasActiveTodos = fmap getAny . getAp . foldMap go
      where
        go obj = Ap $ do
            mdl <- benignReadIORef $ sceneRef obj
            pure $ Any $ mdl ^. _meta.TD._completed

    hdlChange :: (AsReactor c) => Gadget c o TD.TodoCollection ()
    hdlChange = do
        trigger_ k "onChange" ()
        -- logWarn $ pure "App ToggleAll"
        mutateThen k $ do
            s <- get
            a <- lift $ hasActiveTodos (s ^. W._visibleList)
            pure $ getAls $ foldMap (go a) (view W._visibleList s) -- Only modify visible!
      where
        go a obj = Als $ do
            -- lift from ContT to GadgetT
            lift . gadgetWith obj . mutate k $ TD._completed .= not a

app_ :: (AsReactor c, AsJavascript c, AsHTMLElement c)
    => JE.JSRep -> Widget c o App (OnNewTodo (ReactId, J.JSString))
app_ j = do
    todoInputK <- mkReactId "todo-input"
    todoListK <- mkReactId "todo-list"
    let newTodo' = magnifyWidget _newTodo $ todoInput todoListK todoInputK
        appToggleCompleteAll' = magnifyWidget _todos $ mkReactId "complete-all" >>= appToggleCompleteAll
        todoFooter' = magnifyWidget _todos $ mkReactId "todo-footer" >>= TD.todoFooter j
        todosWindow = modifyMarkup (overSurfaceProperties
            (<> [("className", "todo-list")]))
            (W.dynamicCollectionWindow todoListK)
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
                            let ts = s ^. _meta._todos.W._rawCollection
                            if M.null ts
                                then pure ()
                                else bh "section" [ ("key", "main")
                                                        , ("className", "main")
                                                        ] $ do
                                    -- Complete all checkbox
                                    appToggleCompleteAllWin

                                    -- Render the list of todos
                                    magnifiedMeta _todos todosWindow

                                    -- Render the footer
                                    todoFooterWin
                in display win
        gad = finish $ do
            -- FIXME: this is being called on every keystroke in the input!
            -- FIXME: don't update visible if k is from new-todo
            debugIO_ $ putStrLn "__FILE__hello"
            logError $ pure "App Mutated"
            logInfo $ pure "Hello, world!"
            onMutated $ \k ->
                -- ignore updates from the new-todo input box while typing
                if k == todoInputK
                    then pure ()
                    else updateTodos k
    wid `also` (lift gad)

app :: (AsReactor c, AsJavascript c, AsHTMLElement c)
    => JE.JSRep -> Widget c o App r
app j = app_ j >>= (lift . insertTodo')

updateTodos :: (AsReactor c)
    => ReactId -> Gadget c o App ()
updateTodos k = magnifiedEntity _todos $
    mutate k $ W.updateVisibleList todoFilterer todoSorter

todoToggleCompleted :: (AsReactor c)
    => TD.OnTodoToggleComplete (ReactId, UKey) -> Gadget c o App ()
todoToggleCompleted (untag @"OnTodoToggleComplete" -> (k, _)) = updateTodos k

destroyTodo :: (AsReactor c)
    => TD.OnTodoDestroy (ReactId, UKey) -> Gadget c o App ()
destroyTodo (untag @"OnTodoDestroy" -> (k, i)) =
    mutate k $ _todos.W._rawCollection.(at i) .= Nothing

type OnTodoMutated = Tagged "OnTodoMutated"

tickedTodo :: (AsReactor c)
    => OnTodoMutated (ReactId, UKey) -> Gadget c o App ()
tickedTodo (untag @"OnTodoMutated" -> (k, _)) = updateTodos k

insertTodo :: (AsReactor c, AsJavascript c, AsHTMLElement c)
    => OnNewTodo (ReactId, J.JSString) -> Gadget c o App
        (Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)])
insertTodo (untag @"OnNewTodo" -> (k, v)) = do
    s <- getMeta
    let mi = view (_todos.W._rawCollection.to lookupMax) s
        i = case mi of
            Just (i', _) -> largerUKey i'
            Nothing -> zeroUKey
    delegate $ \fire -> do
        -- logDebug $ pure "New todo"
        eo <- doInsertTodo i (TD.Todo v False False)
        case eo of
            Left a -> fire a
            Right obj -> mutate k $ _todos.W._rawCollection.(at i) .= Just obj
  where
    lookupMax = listToMaybe . M.toDescList

doInsertTodo :: (AsJavascript c, AsHTMLElement c, MonadReactor c m)
    => UKey -> TD.Todo -> m
        (Either (Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)]) (Obj TD.Todo))
doInsertTodo i td = mkObj (decorateTodoEvents i <$> todo') td
  where
    todo' = TD.todo
        & chooseWith also $ (onMutated $ pure . pickOnly . Tagged @"OnTodoMutated")


decorateTodoEvents :: UKey -> Which '[TD.OnTodoToggleComplete ReactId, TD.OnTodoDestroy ReactId, OnTodoMutated ReactId]
    -> Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)]
decorateTodoEvents i = which $ cases
    $ decorateOnTodoToggleComplete
    ./ decorateOnTodoDestroy
    ./ decorateOnTodoMutated
    ./ nil
  where
    -- _id avoids using NoMonomorphismRestriction
    _id = id @(Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)])
    decorateOnTodoToggleComplete (untag @"OnTodoToggleComplete" @ReactId -> k) = _id $ pickTag @"OnTodoToggleComplete" (k, i)
    decorateOnTodoDestroy (untag @"OnTodoDestroy" @ReactId -> k) = _id $ pickTag @"OnTodoDestroy" (k, i)
    decorateOnTodoMutated (untag @"OnTodoMutated" @ReactId -> k) = _id $ pickTag @"OnTodoMutated" (k, i)

insertTodo' :: (AsReactor c, AsJavascript c, AsHTMLElement c)
    => OnNewTodo (ReactId, J.JSString) -> Gadget c o App r
insertTodo' newTodoVal = insertTodo newTodoVal
    >>= (injectedK $ totally . finish . todoToggleCompleted . obvious)
    >>= (injectedK $ totally . finish . destroyTodo . obvious)
    >>= (injectedK $ totally . finish . tickedTodo . obvious)
    & fmap impossible
