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
import Glazier.React
import Glazier.React.Framework
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD.Filter

-- data TodoFooter = TodoFooter
--     { activeCount :: Int
--     , completedCount :: Int
--     , currentFilter :: TD.Filter.Filter
--     } deriving G.Generic

-- hdlSetFilter :: MonadReactor m => SceneHandler m p TodoFooter TD.Filter.Filter (Which '[])
-- hdlSetFilter this@(Obj ref its) fltr = terminate' . lift $ do
--     doModifyIORef' ref (my._model.field @"currentFilter" .~ fltr)
--     dirty this

-- hdlSetCounts :: MonadReactor m => SceneHandler m p TodoFooter (Int, Int) (Which '[])
-- hdlSetCounts this@(Obj ref its) (activeCnt, completedCnt) = terminate' . lift $ do
--     doModifyIORef' ref (\s -> s
--         & my._model.field @"activeCount" .~ activeCnt
--         & my._model.field @"completedCount" .~ completedCnt)
--     dirty this

-- todoFooter :: MonadReactor m => Prototype m p TodoFooter (Which '[ClearCompleted])
-- todoFooter = nulPrototype
--             { display = todoDisplay
--             , initializer = trigger' gid "onClick" (pickOnly ClearCompleted)
--             -- , handler = ((. obvious) <$> hdlSetFilter) `orHandler` ((. obvious) <$> hdlSetCounts)
--             }
--   where
--     gid = GadgetId "footer"

-- data ClearCompleted = ClearCompleted

-- todoDisplay :: Monad m => FrameDisplay m TodoFooter ()
-- todoDisplay s = do
--     bh "footer" [("className", "footer")] $ do
--         bh "span" [ ("className", "todo-count")
--                     , ("key", "todo-count")] $ do
--             bh "strong" [("key", "items")]
--                 (s ^. _model.field @"activeCount" . to show . to J.pack . to txt)
--             txt " items left"
--         bh "ul" [("className", "filters")
--                   , ("key", "filters")] $ do
--             bh "li" [("key", "filter-all")] $
--                 bh "a" [ ("href", "#/")
--                          , ("key", "all")
--                          , ("className", JE.classNames
--                             [("selected"
--                             , s ^. _model.field @"currentFilter" == TD.Filter.All)])
--                          ] $
--                 txt "All"
--             txt " "
--             bh "li" [("key", "filter-active")] $
--                 bh "a"
--                 [ ("href", "#/active")
--                 , ("key", "active")
--                 , ("className", JE.classNames
--                     [("selected"
--                     , s ^. _model.field @"currentFilter" == TD.Filter.Active)])
--                 ] $
--                 txt "Active"
--             txt " "
--             bh "li" [("key", "filter-completed")] $
--                 bh "a"
--                     [ ("href", "#/completed")
--                     , ("key", "completed")
--                     , ("className", JE.classNames
--                         [("selected"
--                         , s ^. _model.field @"currentFilter" == TD.Filter.Completed)])
--                     ] $
--                     txt "Completed"
--         if (s ^. _model.field @"completedCount" > 0)
--            then bh' gid s "button"
--                     [("key", "clear-completed"), ("className", "clear-completed")] $
--                             -- , ("onClick", s ^. fireClearCompleted . to JE.toJSR)] $
--                     txt "Clear completed"
--            else mempty
--   where
--     gid = GadgetId "footer"

-- -- -- | This needs to be explictly registered by the Main app
-- -- onHashChange ::  J.JSVal -> MaybeT IO [Action]
-- -- onHashChange = eventHandlerM whenHashChange withHashChange

-- -- -- | Provide split up parts of onHashChange in case the applications
-- -- -- needs to combine other widgets that also uses hashchange event
-- -- whenHashChange :: J.JSVal -> MaybeT IO J.JSString
-- -- whenHashChange evt = do
-- --     newURL <- MaybeT (JE.fromJSR <$> JE.getProperty "newURL" evt)
-- --     let (_, newHash) = J.breakOn "#" newURL
-- --     pure newHash

-- -- -- | Provide split up parts of onHashChange in case the applications
-- -- -- needs to combine other widgets that also uses hashchange event
-- -- withHashChange :: J.JSString -> MaybeT IO [Action]
-- -- withHashChange newHash =
-- --     case newHash of
-- --         "#/active" -> pure [SetFilterAction TD.Filter.Active]
-- --         "#/completed" -> pure [SetFilterAction TD.Filter.Completed]
-- --         _ -> pure [SetFilterAction TD.Filter.All]
