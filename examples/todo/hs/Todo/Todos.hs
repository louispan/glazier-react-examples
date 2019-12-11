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

module Todo.Todos where

import Control.Monad.Extra
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import qualified GHC.Generics as G
import Glazier.React
import qualified JS.DOM as DOM
import Todo.Todo

default (JSString)

data Filter = All | Active | Completed
    deriving (Eq, Show, Ord, G.Generic)

data Todos = Todos
    { filterCriteria :: Filter
    , todoMap :: M.Map ReactId (Obj Todo)
    } deriving (G.Generic)

makeLenses_ ''Todos

footer :: MonadWidget s m => Traversal' s Todos -> m ()
footer this = do
    xs <- model $ this._todoMap
    (completes, actives) <- partitionM isCompleted (snd <$> M.toList xs)
    let completedCount = length completes
        activeCount = length actives

    bh "footer" [("onMount", onMount)] [("className", "footer")] $ do
        bh "span" [] [("className", "todo-count"), ("key", "todo-count")] $ do
            bh "strong" [] [("key", "items")] $
                txt $ fromString $ show activeCount
            txt " items left"
        bh "ul" [] [("className", "filters"), ("key", "filters")] $ do
            bh "li" [] [("key", "filter-all")] $
                bh "a" [] [ ("href", "#/")
                        , ("key", "all")
                        , ("className", classNames
                            [("selected", model $ this._filterCriteria.to (== All))])
                        ] $
                txt "All"
            txt " "
            bh "li" [] [("key", "filter-active")] $
                bh "a" [] [ ("href", "#/active")
                        , ("key", "active")
                        , ("className", classNames
                            [("selected", model $ this._filterCriteria.to (== Active))])
                        ] $
                    txt "Active"
            txt " "
            bh "li" [] [("key", "filter-completed")] $
                bh "a" [] [ ("href", "#/completed")
                        , ("key", "completed")
                        , ("className", classNames
                            [("selected", model $ this._filterCriteria.to (== Completed))])
                        ] $
                    txt "Completed"
        if (completedCount > 0)
           then bh "button" [("onClick", onClearCompletedClicked)]
                        [("key", "clear-completed"), ("className", "clear-completed")] $
                    txt "Clear completed"
           else pure ()
  where
    onMount = mkHandler (const $ pure ()) $ const $ do
        w <- guardJustIO $ fromJS @DOM.Window <$> globalThis `getProperty` "window"
        listenEventTarget w "hashchange" fromHashChange hdlHashChange
    fromHashChange j = do
        o <- guardJust $ fromJS @JSObject j
        newURL <- guardJustIO $ fromJS <$> getProperty o "newURL"
        let (_, newHash) = J.breakOn "#" newURL
        pure newHash
    hdlHashChange newHash = do
        noisyMutate $ this._filterCriteria .= case newHash of
            "#/active" -> Active
            "#/completed" -> Completed
            _ -> All
    isCompleted obj = completed <$> (readObj obj)
    onClearCompletedClicked = mkHandler' (const $ pure ()) $ const $ do
        xs <- model $ this._todoMap
        actives <- filterM (fmap not . isCompleted . snd) (M.toList xs)
        noisyMutate $ this._todoMap .= M.fromList actives

todoItems :: MonadWidget s m => Traversal' s Todos -> m ()
todoItems this = do
    xs <- model $ this._todoMap
    ftr <- model $ this._filterCriteria
    ys <- filterM (isVisible ftr) (snd <$> M.toList xs)
    traverse_ displayObj ys
  where
    isVisible ftr obj = do
        completed' <- completed <$> readObj obj
        case (ftr, completed') of
            (All, _) -> pure True
            (Active, False) -> pure True
            (Completed, True) -> pure True
            _ -> pure False

mkTodo :: MonadWidget s m => Traversal' s Todos -> ReactId -> JSString -> m ()
mkTodo this k v = do
    onTodoDestroy' <- codify onTodoDestroy
    let todo' = (`runObserverT` (instruct . onTodoDestroy')) $ todo id
    obj <- mkObj todo' "todo" $ Todo v False NotEditing
    noisyMutate $ this._todoMap %= M.insert k obj
  where
    onTodoDestroy (untag' @"TodoDestroy" -> ()) =
        noisyMutate $ this._todoMap %= M.delete k
