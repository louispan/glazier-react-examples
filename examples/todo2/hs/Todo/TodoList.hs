{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Todo.TodoList where

import Control.Monad.Extra
import Data.Foldable
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified Glazier.DOM as DOM
import Glazier.React
import Todo.Todo

default (JSString)

data Filter = All | Active | Completed
    deriving (Eq, Show, Ord, G.Generic)

data TodoList = TodoList
    { filterCriteria :: Filter
    , todos :: [Obj Todo]
    } deriving (G.Generic)

makeLenses_ ''TodoList

footer :: MonadWidget s m => Traversal' s TodoList -> m ()
footer this = do
    initConstructor $ do
        w <- guardJustM $ pure DOM.globalWindow
        listenEventTarget w "hashchange" fromHashChange hdlHashChange

    xs <- model $ this._todos
    (completes, actives) <- partitionM isCompleted xs
    let completedCount = length completes
        activeCount = length actives

    bh "footer" [] [("className", "footer")] $ do
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
    isCompleted obj = completed <$> (readObj obj)
    fromHashChange j = do
        newURL <- guardJustIO $ fromJS <$> getProperty j "newURL"
        let (_, newHash) = J.breakOn "#" newURL
        pure newHash
    hdlHashChange newHash = do
        noisyMutate $ this._filterCriteria .= case newHash of
            "#/active" -> Active
            "#/completed" -> Completed
            _ -> All
    onClearCompletedClicked = mkHandler' (const $ pure ()) $ const $ do
        xs <- model $ this._todos
        actives <- filterM (fmap not. isCompleted) xs
        noisyMutate $ this._todos .= actives

todoList :: MonadWidget s m => Traversal' s TodoList -> m ()
todoList this = do
    xs <- model $ this._todos
    ftr <- model $ this._filterCriteria
    ys <- filterM (isVisible ftr) xs
    traverse_ displayTodo ys
  where
    isVisible ftr obj = do
        completed' <- completed <$> readObj obj
        case (ftr, completed') of
            (All, _) -> pure True
            (Active, False) -> pure True
            (Completed, True) -> pure True
            _ -> pure False
    displayTodo obj = bh "ul" [] [] $ displayObj obj
