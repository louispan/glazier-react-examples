{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Footer where

import Control.Arrow
import Control.Lens
import Control.Lens.Misc
import Control.Monad.Trans.Class
import qualified Data.JSString as J
import qualified GHC.Generics as G
import Glazier.React.Framework
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD

data Footer = Footer
    { activeCount :: Int
    , completedCount :: Int
    , currentFilter :: TD.Filter
    } deriving G.Generic

makeLenses_ ''Footer

hdlSetFilter :: MonadReactor m => TD.Filter -> MethodT (Scene p m Footer) m ()
hdlSetFilter fltr = readrT' $ \this@Obj{..} -> lift $ do
    doModifyIORef' self (my._model._currentFilter .~ fltr)
    dirty this


hdlSetCounts :: MonadReactor m => (Int, Int) -> MethodT (Scene p m Footer) m ()
hdlSetCounts (activeCnt, completedCnt) = readrT' $ \this@Obj{..} -> lift $ do
    doModifyIORef' self (
        my._model._activeCount .~ activeCnt
        >>> my._model._completedCount .~ completedCnt)
    dirty this

todoFooter :: MonadReactor m => Prototype p Footer m (ClearCompleted)
todoFooter = mempty
            { display = todoDisplay
            , initializer = trigger' gid "onClick" ClearCompleted
            }
  where
    gid = GadgetId "footer"

data ClearCompleted = ClearCompleted

todoDisplay :: Monad m => FrameDisplay Footer m ()
todoDisplay s =
    bh "footer" [("className", "footer")] $ do
        bh "span" [ ("className", "todo-count")
                    , ("key", "todo-count")] $ do
            bh "strong" [("key", "items")]
                (s ^. _model._activeCount . to show . to J.pack . to txt)
            txt " items left"
        bh "ul" [("className", "filters")
                  , ("key", "filters")] $ do
            bh "li" [("key", "filter-all")] $
                bh "a" [ ("href", "#/")
                         , ("key", "all")
                         , ("className", JE.classNames
                            [("selected"
                            , s ^. _model._currentFilter == TD.All)])
                         ] $
                txt "All"
            txt " "
            bh "li" [("key", "filter-active")] $
                bh "a"
                [ ("href", "#/active")
                , ("key", "active")
                , ("className", JE.classNames
                    [("selected"
                    , s ^. _model._currentFilter == TD.Active)])
                ] $
                txt "Active"
            txt " "
            bh "li" [("key", "filter-completed")] $
                bh "a"
                    [ ("href", "#/completed")
                    , ("key", "completed")
                    , ("className", JE.classNames
                        [("selected"
                        , s ^. _model._currentFilter == TD.Completed)])
                    ] $
                    txt "Completed"
        if (s ^. _model._completedCount > 0)
           then bh' gid s "button"
                    [("key", "clear-completed"), ("className", "clear-completed")] $
                            -- , ("onClick", s ^. fireClearCompleted . to JE.toJSR)] $
                    txt "Clear completed"
           else mempty
  where
    gid = GadgetId "footer"
