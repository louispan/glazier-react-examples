{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
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
import qualified Glazier.React as Z
import qualified Glazier.React.Framework as Z
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD.Filter

data TodoFooter = TodoFooter
    { activeCount :: Int
    , completedCount :: Int
    , currentFilter :: TD.Filter.Filter
    } deriving G.Generic

hdlSetFilter :: Z.MonadReactor m => Z.SceneHandler m v TodoFooter TD.Filter.Filter (Which '[])
hdlSetFilter this@(Z.Obj ref its) fltr = Z.terminate' . lift $ do
    Z.doModifyIORef' ref (its.Z.model.field @"currentFilter" .~ fltr)
    Z.dirty this

hdlSetCounts :: Z.MonadReactor m => Z.SceneHandler m v TodoFooter (Int, Int) (Which '[])
hdlSetCounts this@(Z.Obj ref its) (activeCnt, completedCnt) = Z.terminate' . lift $ do
    Z.doModifyIORef' ref (\s -> s
        & its.Z.model.field @"activeCount" .~ activeCnt
        & its.Z.model.field @"completedCount" .~ completedCnt)
    Z.dirty this

todoFooter :: Z.MonadReactor m => Z.Prototype m v TodoFooter (Which '[ClearCompleted])
todoFooter = Z.nulPrototype
            { Z.display = todoDisplay
            , Z.initializer = Z.trigger' gid "onClick" (pickOnly ClearCompleted)
            -- , Z.handler = ((. obvious) <$> hdlSetFilter) `Z.orHandler` ((. obvious) <$> hdlSetCounts)
            }
  where
    gid = Z.GadgetId "footer"

data ClearCompleted = ClearCompleted

todoDisplay :: Monad m => Z.FrameDisplay m TodoFooter ()
todoDisplay s = do
    Z.bh "footer" [("className", "footer")] $ do
        Z.bh "span" [ ("className", "todo-count")
                    , ("key", "todo-count")] $ do
            Z.bh "strong" [("key", "items")]
                (s ^. Z.model.field @"activeCount" . to show . to J.pack . to Z.txt)
            Z.txt " items left"
        Z.bh "ul" [("className", "filters")
                  , ("key", "filters")] $ do
            Z.bh "li" [("key", "filter-all")] $
                Z.bh "a" [ ("href", "#/")
                         , ("key", "all")
                         , ("className", JE.classNames
                            [("selected"
                            , s ^. Z.model.field @"currentFilter" == TD.Filter.All)])
                         ] $
                Z.txt "All"
            Z.txt " "
            Z.bh "li" [("key", "filter-active")] $
                Z.bh "a"
                [ ("href", "#/active")
                , ("key", "active")
                , ("className", JE.classNames
                    [("selected"
                    , s ^. Z.model.field @"currentFilter" == TD.Filter.Active)])
                ] $
                Z.txt "Active"
            Z.txt " "
            Z.bh "li" [("key", "filter-completed")] $
                Z.bh "a"
                    [ ("href", "#/completed")
                    , ("key", "completed")
                    , ("className", JE.classNames
                        [("selected"
                        , s ^. Z.model.field @"currentFilter" == TD.Filter.Completed)])
                    ] $
                    Z.txt "Completed"
        if (s ^. Z.model.field @"completedCount" > 0)
           then Z.bh' gid s "button"
                    [("key", "clear-completed"), ("className", "clear-completed")] $
                            -- , ("onClick", s ^. fireClearCompleted . to JE.toJSR)] $
                    Z.txt "Clear completed"
           else mempty
  where
    gid = Z.GadgetId "footer"

-- -- | This needs to be explictly registered by the Main app
-- onHashChange ::  J.JSVal -> MaybeT IO [Action]
-- onHashChange = Z.eventHandlerM whenHashChange withHashChange

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
