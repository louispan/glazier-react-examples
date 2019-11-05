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
{-# LANGUAGE ViewPatterns #-}

module Todo.App where

import Control.Monad.Extra
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Widgets.Input
import qualified JS.DOM as DOM
import Todo.Todo
import Todo.Todos

default (JSString)

data App = App
    { newTodo :: J.JSString
    , todos :: Todos
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

todoNewInput :: (MonadWidget s m, MonadObserver' (Tagged "NewTodo" JSString) m)
    => Traversal' s App -> m ()
todoNewInput this = input (this._newTodo)
    [("onKeyDown", onKeyDown), ("onMount", onMount)]
    [("className", "new-todo"), ("placeholder", "What needs to be done?")]
  where
    onKeyDown = mkHandler' fromKeyDown (observe' . Tagged @"NewTodo")
    -- Manipulate the DOM input directly to avoid race conditions with lazy GHCJS
    fromKeyDown :: SyntheticEvent -> MaybeT IO JSString
    fromKeyDown evt = do
        t <- guardJustIO $ DOM.target evt
        case DOM.key <$> e of
            Just "Enter" -> do
                v <- guardJustIO $ fromJS <$> getProperty t "value"
                t `setProperty` ("value", "")
                let v' = J.strip v
                if J.null v'
                    then empty
                    else pure v'
            Just "Escape" -> do
                t `setProperty` ("value", "")
                empty
            _ -> empty
      where
        e = viaJS @SyntheticKeyboardEvent evt

    onMount = mkHandler pure hdlMount
    hdlMount j = do
        j' <- guardJustM $ pure $ viaJS @DOM.HTMLElement j
        DOM.focus j'

toggleCompleteAll :: MonadWidget s m => Traversal' s App -> m ()
toggleCompleteAll this = lf "input" [("onChange", onChange)]
    [ ("type", "checkbox")
    , ("className", "toggle-all")
    , ("checked", (toJS . (==) 0) <$> activeTodoCount (this._todos._todoMap))
    ]
  where
    onChange = mkHandler' fromChange handleChange
    fromChange j = do
        t <- guardJustIO $ DOM.target j
        guardJustIO . fmap fromJS . (`getProperty` "checked") $ t
    handleChange checked = do
        xs <- model (this._todos._todoMap)
        traverse_ (`shall` setComplete checked) xs
    setComplete checked = noisyMutate $ _completed .= checked

activeTodoCount :: MonadGadget s m => Traversal' s (M.Map ReactId (Obj Todo)) -> m Int
activeTodoCount this = do
    tds <- model this
    length <$> filterM isActive (snd <$> M.toList tds)
  where
    isActive obj = (not . completed) <$> (readObj obj)

app :: (MonadWidget s m)
    => Traversal' s App -> m ()
app this = do
    (`runObserverT` onNewTodo)
        $ bh "div" [] [] $ do
            bh "header" [] [("className", "header")] $ do
                bh "h1" [] [] (txt "todos")
                todoNewInput this

            whenM (model $ this._todos._todoMap.to null.to not) $ do
                bh "section" [] [("className", "main")] $ do
                    toggleCompleteAll this
                    lf "label" [] [("htmlFor","toggle-all")]
                    bh "ul" [] [("className", "todo-list")] $ do
                        todoItems (this._todos)
                footer (this._todos)
  where
    onNewTodo (untag' @"NewTodo" @JSString -> v) = do
        k <- mkReactId
        mkTodo (this._todos) k v

