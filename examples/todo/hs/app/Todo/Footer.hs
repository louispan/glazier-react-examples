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
import qualified Glazier.React.Framework as F
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD.Filter

data TodoFooter = TodoFooter
    { activeCount :: Int
    , completedCount :: Int
    , currentFilter :: TD.Filter.Filter
    } deriving G.Generic

todoFooter ::
    ( F.MonadReactor m
    , HasItem' TodoFooter s
    , HasItem' TodoFooter i
    )
    => F.Prototype m v i s
        (Many '[TodoFooter])
        (Many '[TodoFooter])
        (Which '[ClearCompleted])
        (Which '[SetFilter, SetCounts])
        (Which '[])
todoFooter =
    let p = F.nulPrototype
            { F.builder = F.build @TodoFooter
            , F.display = todoDisplay
            , F.activator = onChange
            , F.handler = ((. obvious) <$> hdlSetFilter) `F.orHandler` ((. obvious) <$> hdlSetCounts)
            }
    in F.toItemPrototype p

todoDisplay :: (Monad m)
    => F.FrameDisplay m TodoFooter ()
todoDisplay s = do
    R.branch' "footer" [("className", "footer")] $ do
        R.branch' "span" [ ("className", "todo-count")
                    , ("key", "todo-count")] $ do
            R.branch' "strong" [("key", "items")]
                (s ^. F.model.field @"activeCount" . to show . to J.pack . to R.txt)
            R.txt " items left"
        R.branch' "ul" [("className", "filters")
                  , ("key", "filters")] $ do
            R.branch' "li" [("key", "filter-all")] $
                R.branch' "a" [ ("href", "#/")
                         , ("key", "all")
                         , ("className", JE.classNames
                            [("selected"
                            , s ^. F.model.field @"currentFilter" == TD.Filter.All)])
                         ] $
                R.txt "All"
            R.txt " "
            R.branch' "li" [("key", "filter-active")] $
                R.branch' "a"
                [ ("href", "#/active")
                , ("key", "active")
                , ("className", JE.classNames
                    [("selected"
                    , s ^. F.model.field @"currentFilter" == TD.Filter.Active)])
                ] $
                R.txt "Active"
            R.txt " "
            R.branch' "li" [("key", "filter-completed")] $
                R.branch' "a"
                    [ ("href", "#/completed")
                    , ("key", "completed")
                    , ("className", JE.classNames
                        [("selected"
                        , s ^. F.model.field @"currentFilter" == TD.Filter.Completed)])
                    ] $
                    R.txt "Completed"
        if (s ^. F.model.field @"completedCount" > 0)
           then F.branch "button" (F.getListeners i s)
                    [("key", "clear-completed"), ("className", "clear-completed")] $
                            -- , ("onClick", s ^. fireClearCompleted . to JE.toJS')] $
                    R.txt "Clear completed"
           else mempty
  where
    i = F.GadgetId "footer"

data ClearCompleted = ClearCompleted

onChange ::
    ( F.MonadReactor m
    ) => F.SceneActivator m v s (Which '[ClearCompleted])
onChange = F.trigger i "onClick" (const $ pure ()) (const $ pickOnly ClearCompleted)
  where
    i = F.GadgetId "footer"

data SetFilter = SetFilter TD.Filter.Filter

hdlSetFilter ::
    (F.MonadReactor m)
    => F.SceneHandler m v TodoFooter SetFilter (Which '[])
hdlSetFilter this@(F.Obj ref its) (SetFilter fltr) = F.terminate' . lift $ do
    F.doModifyIORef' ref (its.F.model.field @"currentFilter" .~ fltr)
    F.rerender' this

data SetCounts = SetCounts Int Int

hdlSetCounts ::
    (F.MonadReactor m)
    => F.SceneHandler m v TodoFooter SetCounts (Which '[])
hdlSetCounts this@(F.Obj ref its) (SetCounts activeCnt completedCnt) = F.terminate' . lift $ do
    F.doModifyIORef' ref (\s -> s
        & its.F.model.field @"activeCount" .~ activeCnt
        & its.F.model.field @"completedCount" .~ completedCnt)
    F.rerender' this

-- -- | This needs to be explictly registered by the Main app
-- onHashChange ::  J.JSVal -> MaybeT IO [Action]
-- onHashChange = R.eventHandlerM whenHashChange withHashChange

-- -- | Provide split up parts of onHashChange in case the applications
-- -- needs to combine other widgets that also uses hashchange event
-- whenHashChange :: J.JSVal -> MaybeT IO J.JSString
-- whenHashChange evt = do
--     newURL <- MaybeT (JE.fromJS' <$> JE.getProperty "newURL" evt)
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
