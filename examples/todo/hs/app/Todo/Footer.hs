{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Footer where

import Control.Lens
import Control.Monad.Trans.Class
import Data.Diverse.Profunctor
import Data.Generics.Product
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified Glazier.React as R
import qualified Glazier.React.Framework as R
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD.Filter

data TodoFooter = TodoFooter
    { activeCount :: Int
    , completedCount :: Int
    , currentFilter :: TD.Filter.Filter
    } deriving G.Generic

hdlSetFilter ::
    (R.MonadReactor m)
    => R.SceneHandler m v TodoFooter TD.Filter.Filter (Which '[])
hdlSetFilter this@(R.Obj ref its) fltr = R.terminate' . lift $ do
    R.doModifyIORef' ref (its.R.model.field @"currentFilter" .~ fltr)
    R.rerender' this

hdlSetCounts ::
    (R.MonadReactor m)
    => R.SceneHandler m v TodoFooter (Int, Int) (Which '[])
hdlSetCounts this@(R.Obj ref its) (activeCnt, completedCnt) = R.terminate' . lift $ do
    R.doModifyIORef' ref (\s -> s
        & its.R.model.field @"activeCount" .~ activeCnt
        & its.R.model.field @"completedCount" .~ completedCnt)
    R.rerender' this

todoFooter ::
    ( R.MonadReactor m
    )
    => R.Prototype m v TodoFooter (Which '[ClearCompleted])
todoFooter = R.nulPrototype
            { R.display = todoDisplay
            , R.initializer = onChange
            -- , R.handler = ((. obvious) <$> hdlSetFilter) `R.orHandler` ((. obvious) <$> hdlSetCounts)
            }

todoDisplay :: (Monad m)
    => R.FrameDisplay m TodoFooter ()
todoDisplay s = do
    R.bh "footer" [("className", "footer")] $ do
        R.bh "span" [ ("className", "todo-count")
                    , ("key", "todo-count")] $ do
            R.bh "strong" [("key", "items")]
                (s ^. R.model.field @"activeCount" . to show . to J.pack . to R.txt)
            R.txt " items left"
        R.bh "ul" [("className", "filters")
                  , ("key", "filters")] $ do
            R.bh "li" [("key", "filter-all")] $
                R.bh "a" [ ("href", "#/")
                         , ("key", "all")
                         , ("className", JE.classNames
                            [("selected"
                            , s ^. R.model.field @"currentFilter" == TD.Filter.All)])
                         ] $
                R.txt "All"
            R.txt " "
            R.bh "li" [("key", "filter-active")] $
                R.bh "a"
                [ ("href", "#/active")
                , ("key", "active")
                , ("className", JE.classNames
                    [("selected"
                    , s ^. R.model.field @"currentFilter" == TD.Filter.Active)])
                ] $
                R.txt "Active"
            R.txt " "
            R.bh "li" [("key", "filter-completed")] $
                R.bh "a"
                    [ ("href", "#/completed")
                    , ("key", "completed")
                    , ("className", JE.classNames
                        [("selected"
                        , s ^. R.model.field @"currentFilter" == TD.Filter.Completed)])
                    ] $
                    R.txt "Completed"
        if (s ^. R.model.field @"completedCount" > 0)
           then R.bh' i s "button"
                    [("key", "clear-completed"), ("className", "clear-completed")] $
                            -- , ("onClick", s ^. fireClearCompleted . to JE.toJSR)] $
                    R.txt "Clear completed"
           else mempty
  where
    i = R.GadgetId "footer"

data ClearCompleted = ClearCompleted

onChange ::
    ( R.MonadReactor m
    ) => R.SceneInitializer m v s (Which '[ClearCompleted])
onChange = R.trigger i "onClick" (const $ pure ()) (const $ pickOnly ClearCompleted)
  where
    i = R.GadgetId "footer"

-- -- | This needs to be explictly registered by the Main app
-- onHashChange ::  J.JSVal -> MaybeT IO [Action]
-- onHashChange = R.eventHandlerM whenHashChange withHashChange

-- -- | Provide split up parts of onHashChange in case the applications
-- -- needs to combine other widgets that also uses hashchange event
-- whenHashChange :: J.JSVal -> MaybeT IO J.JSString
-- whenHashChange evt = do
--     newURL <- MaybeT (JE.fromJSR <$> JE.getProperty "newURL" evt)
--     let (_, newHash) = J.breakOn "#" newURL
--     pure newHash

-- -- | Provide split up parts of onHashChange in case the applications
-- -- needs to combine other widgets that also uses hashchange event
-- withHashChange :: J.JSString -> MaybeT IO [Action]
-- withHashChange newHash =
--     case newHash of
--         "#/active" -> pure [SetFilterAction TD.Filter.Active]
--         "#/completed" -> pure [SetFilterAction TD.Filter.Completed]
--         _ -> pure [SetFilterAction TD.Filter.All]
