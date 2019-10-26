{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Todo.App where

import Data.Foldable
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified Glazier.DOM as DOM
import Glazier.React
import Glazier.React.Widgets.Input
import Todo.Todo
import Todo.TodoList

default (JSString)

-- -- | Just use map order
-- todoSorter :: Applicative m => srt -> a -> a -> m Ordering
-- todoSorter _ _ _ = pure LT

-- todoFilterer :: TD.Filter -> Obj TD.Todo -> Benign IO Bool
-- todoFilterer ftr td = do
--     td' <- meta <$> (benignReadIORef $ sceneRef td)
--     pure $ case ftr of
--         TD.All -> True
--         TD.Active -> not $ TD.completed $ td'
--         TD.Completed -> TD.completed $ td'

data App = App
    { newTodo :: J.JSString
    , todoList :: TodoList
    } deriving (G.Generic)

makeLenses_ ''App

-- instance MonadIO m => A.AToJSON (Benign m) App
-- instance (Applicative m, AsJavascript c, AsHTMLElement c, MonadReactant c m) => A.AFromJSON (ExceptT (Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)]) m) App where
--     aparseJSON = A.withObject "App" $ \v -> fmap initialize (aparse' v)
--       where
--         aparse' v = getCompose $ (,)
--             <$> (Compose $ A.aparseField v "newTodo")
--             <*> (Compose $ A.aparseField v "todos")
--         initialize m = do
--             (v, xs) <- m
--             xs' <- initialize' xs
--             pure $ App v xs'
--         initialize' :: (AsJavascript c, AsHTMLElement c, MonadReactant c m) => W.DynamicCollection TD.Filter () UKey TD.Todo -> (ExceptT (Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)]) m) TD.TodoCollection
--         initialize' W.DynamicCollection {..} = do
--             xs <- M.traverseWithKey (\k -> ExceptT . doInsertTodo k) rawCollection
--             let xs' = W.DynamicCollection filterCriteria sortCriteria [] xs
--             lift $ evalBenignIO $ execStateT (W.updateVisibleList todoFilterer todoSorter) xs'

-- type OnNewTodo = Tagged "OnNewTodo"

todoNewInput :: (MonadWidget s m, MonadObserver' (Tagged "OnNewTodo" JSString) m)
    => ReactId -> Traversal' s App -> m ()
todoNewInput scratchId this = input (this._newTodo)
    [("onKeyDown", onKeyDown), ("ref", onRef)]
    [("className", "new-todo"), ("placeholder", "What needs to be done?")]
  where
    onKeyDown = mkHandler' fromKeyDown (observe' . Tagged @"OnNewTodo")
    -- Manipulate the DOM input directly to avoid race conditions with lazy GHCJS
    fromKeyDown :: DOM.SyntheticEvent -> MaybeT IO JSString
    fromKeyDown evt = case DOM.key <$> e of
        Just "Enter" -> do
            v <- fromJustIO $ fromJS <$> getProperty t "value"
            t `setProperty` "value" ""
            let v' = J.strip v
            if J.null v'
                then empty
                else pure v'
        Just "Escape" -> do
            t `setProperty` "value" ""
            empty
        _ -> empty
      where
        t = DOM.target evt
        e = viaJS @DOM.SyntheticKeyboardEvent evt

    onRef = mkHandler pure hdlRef

    hdlRef j = do
        scratchXTimes 1 scratchId "newTodoFocused" $ do
            j' <- fromJustM $ pure $ viaJS @DOM.HTMLElement j
            DOM.focus j'

toggleCompleteAll :: MonadWidget s m => Traversal' s App -> m ()
toggleCompleteAll this = lf inputComponent [("onChange", onChange)]
    [ ("type", "checkbox")
    , ("className", "toggle-all")
    , ("checked", isAllCompleted)
    ]
  where
    -- allCompleted :: MonadGadget s m => m (Maybe JSVal)
    isAllCompleted = (Just . toJS) <$> (model (this._todoList._todos) >>= hasNoActiveTodos)

    -- hasActiveTodos :: MonadGadget s m => [Obj Todo] -> m Bool
    hasNoActiveTodos tds = do
        tds' <- filterM isActive tds
        pure $ null tds'
      where
        isActive obj = (not . completed) <$> (readObj obj)

    onChange = mkHandler' fromChange handleChange
    fromChange = fromJustIO . fmap fromJS . (`getProperty` "checked") . DOM.target
    handleChange checked = do
        xs <- model (this._todoList._todos)
        traverse_ (`shall` setComplete checked) xs
    setComplete checked = noisyMutate $ _completed .= checked

activeTodoCount :: MonadModel s m => Traversal' s App -> Int
activeTodoCount


app :: (MonadWidget s m, MonadObserver' (Tagged "OnNewTodo" JSString) m)
    => ReactId -> Traversal' s App -> m ()
app todoInputId this = do
    bh "div" [] [] $ do
        bh "header" [] [("className", "header")] $ do
            bh "h1" [] [] (txt "todos")
            todoNewInput todoInputId this
        xs <- model (this._todoList._todos)
        when (not $ null xs) $ do
            bh "section" [] [("className", "main")] $ do
                toggleCompleteAll this
                lf "label" [] [("htmlFor","toggle-all")]
                bh "ul" [] [("className", "todo-list")] $ do
                    pure ()


-- app_ :: (AsReactant c, AsJavascript c, AsHTMLElement c)
--     => JE.JSRep -> Widget c o App (OnNewTodo (ReactId, J.JSString))
-- app_ j = do
--     todoInputK <- mkReactId "todo-input"
--     todoListK <- mkReactId "todo-list"
--     let newTodo' = magnifyWidget _newTodo $ todoInput todoListK todoInputK
--         appToggleCompleteAll' = magnifyWidget _todos $ mkReactId "complete-all" >>= appToggleCompleteAll
--         todoFooter' = magnifyWidget _todos $ mkReactId "todo-footer" >>= TD.todoFooter j
--         todosWindow = modifyMarkup (overSurfaceProperties
--             (<> [("className", "todo-list")]))
--             (W.dynamicCollectionWindow todoListK)
--         wid = withWindow newTodo' $ \newTodoWin ->
--             withWindow appToggleCompleteAll' $ \appToggleCompleteAllWin ->
--             withWindow todoFooter' $ \todoFooterWin ->
--                 let win = do
--                         s <- ask
--                         bh "div" [("key", "app")] $ do
--                             bh "header" [("key", "header"), ("className", "header")] $ do
--                                 bh "h1" [("key", "heading")] (txt "todos")
--                                 newTodoWin

--                             -- only render if there are todos
--                             let ts = s ^. _meta._todos.W._rawCollection
--                             if M.null ts
--                                 then pure ()
--                                 else bh "section" [ ("key", "main")
--                                                         , ("className", "main")
--                                                         ] $ do
--                                     -- Complete all checkbox
--                                     appToggleCompleteAllWin

--                                     -- Render the list of todos
--                                     magnifiedMeta _todos todosWindow

--                                     -- Render the footer
--                                     todoFooterWin
--                 in display win
--         gad = finish $ do
--             -- FIXME: this is being called on every keystroke in the input!
--             -- FIXME: don't update visible if k is from new-todo
--             debugIO_ $ putStrLn "__FILE__hello"
--             logError $ pure "App Mutated"
--             logInfo $ pure "Hello, world!"
--             onMutated $ \k ->
--                 -- ignore updates from the new-todo input box while typing
--                 if k == todoInputK
--                     then pure ()
--                     else updateTodos k
--     wid `also` (lift gad)

-- app :: (AsReactant c, AsJavascript c, AsHTMLElement c)
--     => JE.JSRep -> Widget c o App r
-- app j = app_ j >>= (lift . insertTodo')

-- updateTodos :: (AsReactant c)
--     => ReactId -> Gadget c o App ()
-- updateTodos k = magnifiedEntity _todos $
--     mutate k $ W.updateVisibleList todoFilterer todoSorter

-- todoToggleCompleted :: (AsReactant c)
--     => TD.OnTodoToggleComplete (ReactId, UKey) -> Gadget c o App ()
-- todoToggleCompleted (untag @"OnTodoToggleComplete" -> (k, _)) = updateTodos k

-- destroyTodo :: (AsReactant c)
--     => TD.OnTodoDestroy (ReactId, UKey) -> Gadget c o App ()
-- destroyTodo (untag @"OnTodoDestroy" -> (k, i)) =
--     mutate k $ _todos.W._rawCollection.(at i) .= Nothing

-- type OnTodoMutated = Tagged "OnTodoMutated"

-- tickedTodo :: (AsReactant c)
--     => OnTodoMutated (ReactId, UKey) -> Gadget c o App ()
-- tickedTodo (untag @"OnTodoMutated" -> (k, _)) = updateTodos k

-- insertTodo :: (AsReactant c, AsJavascript c, AsHTMLElement c)
--     => OnNewTodo (ReactId, J.JSString) -> Gadget c o App
--         (Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)])
-- insertTodo (untag @"OnNewTodo" -> (k, v)) = do
--     s <- getMeta
--     let mi = view (_todos.W._rawCollection.to lookupMax) s
--         i = case mi of
--             Just (i', _) -> la
--             Nothing -> zeroUKey
--     delegate $ \fire -> do
--         -- logDebug $ pure "New todo"
--         eo <- doInsertTodo i (TD.Todo v False False)
--         case eo of
--             Left a -> fire a
--             Right obj -> mutate k $ _todos.W._rawCollection.(at i) .= Just obj
--   where
--     lookupMax = listToMaybe . M.toDescList

-- doInsertTodo :: (AsJavascript c, AsHTMLElement c, MonadReactant c m)
--     => UKey -> TD.Todo -> m
--         (Either (Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)]) (Obj TD.Todo))
-- doInsertTodo i td = mkObj (decorateTodoEvents i <$> todo') td
--   where
--     todo' = TD.todo
--         & chooseWith also $ (onMutated $ pure . pickOnly . Tagged @"OnTodoMutated")


-- decorateTodoEvents :: UKey -> Which '[TD.OnTodoToggleComplete ReactId, TD.OnTodoDestroy ReactId, OnTodoMutated ReactId]
--     -> Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)]
-- decorateTodoEvents i = which $ cases
--     $ decorateOnTodoToggleComplete
--     ./ decorateOnTodoDestroy
--     ./ decorateOnTodoMutated
--     ./ nil
--   where
--     -- _id avoids using NoMonomorphismRestriction
--     _id = id @(Which '[TD.OnTodoToggleComplete (ReactId, UKey), TD.OnTodoDestroy (ReactId, UKey), OnTodoMutated (ReactId, UKey)])
--     decorateOnTodoToggleComplete (untag @"OnTodoToggleComplete" @ReactId -> k) = _id $ pickTag @"OnTodoToggleComplete" (k, i)
--     decorateOnTodoDestroy (untag @"OnTodoDestroy" @ReactId -> k) = _id $ pickTag @"OnTodoDestroy" (k, i)
--     decorateOnTodoMutated (untag @"OnTodoMutated" @ReactId -> k) = _id $ pickTag @"OnTodoMutated" (k, i)

-- insertTodo' :: (AsReactant c, AsJavascript c, AsHTMLElement c)
--     => OnNewTodo (ReactId, J.JSString) -> Gadget c o App r
-- insertTodo' newTodoVal = insertTodo newTodoVal
--     >>= (injectedK $ totally . finish . todoToggleCompleted . obvious)
--     >>= (injectedK $ totally . finish . destroyTodo . obvious)
--     >>= (injectedK $ totally . finish . tickedTodo . obvious)
--     & fmap impossible

