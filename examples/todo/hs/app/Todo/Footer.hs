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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Footer where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Generics.Product
import qualified Data.JSString as J
import qualified GHC.Generics as G
import Glazier.React.Framework
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD.Filter

data TodoFooter = TodoFooter
    { activeCount :: Int
    , completedCount :: Int
    , currentFilter :: TD.Filter.Filter
    } deriving G.Generic

hdlSetFilter :: MonadReactor m => TD.Filter.Filter -> Delegate (Scene p m TodoFooter) m ()
hdlSetFilter fltr = delegate' $ \this@Obj{..} -> lift $ do
    doModifyIORef' self (my._model.field @"currentFilter" .~ fltr)
    dirty this

hdlSetCounts :: MonadReactor m => (Int, Int) -> Delegate (Scene p m TodoFooter) m ()
hdlSetCounts (activeCnt, completedCnt) = delegate' $ \this@Obj{..} -> lift $ do
    doModifyIORef' self (\me -> me
        & my._model.field @"activeCount" .~ activeCnt
        & my._model.field @"completedCount" .~ completedCnt)
    dirty this

todoFooter :: MonadReactor m => Prototype p TodoFooter m (ClearCompleted)
todoFooter = mempty
            { display = todoDisplay
            , initializer = trigger' gid "onClick" ClearCompleted
            -- , handler = ((. obvious) <$> hdlSetFilter) `orHandler` ((. obvious) <$> hdlSetCounts)
            }
  where
    gid = GadgetId "footer"

data ClearCompleted = ClearCompleted

todoDisplay :: Monad m => FrameDisplay TodoFooter m ()
todoDisplay = method' $ \s -> do
    bh "footer" [("className", "footer")] $ do
        bh "span" [ ("className", "todo-count")
                    , ("key", "todo-count")] $ do
            bh "strong" [("key", "items")]
                (s ^. _model.field @"activeCount" . to show . to J.pack . to txt)
            txt " items left"
        bh "ul" [("className", "filters")
                  , ("key", "filters")] $ do
            bh "li" [("key", "filter-all")] $
                bh "a" [ ("href", "#/")
                         , ("key", "all")
                         , ("className", JE.classNames
                            [("selected"
                            , s ^. _model.field @"currentFilter" == TD.Filter.All)])
                         ] $
                txt "All"
            txt " "
            bh "li" [("key", "filter-active")] $
                bh "a"
                [ ("href", "#/active")
                , ("key", "active")
                , ("className", JE.classNames
                    [("selected"
                    , s ^. _model.field @"currentFilter" == TD.Filter.Active)])
                ] $
                txt "Active"
            txt " "
            bh "li" [("key", "filter-completed")] $
                bh "a"
                    [ ("href", "#/completed")
                    , ("key", "completed")
                    , ("className", JE.classNames
                        [("selected"
                        , s ^. _model.field @"currentFilter" == TD.Filter.Completed)])
                    ] $
                    txt "Completed"
        if (s ^. _model.field @"completedCount" > 0)
           then bh' gid s "button"
                    [("key", "clear-completed"), ("className", "clear-completed")] $
                            -- , ("onClick", s ^. fireClearCompleted . to JE.toJSR)] $
                    txt "Clear completed"
           else mempty
  where
    gid = GadgetId "footer"
