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

module Todo.Footer
    ( Footer(..)
    , _activeCount
    , _completedCount
    , _currentFilter
    -- , hdlSetFilter
    -- , hdlSetCounts
    , ClearCompleted
    , todoFooter
    ) where

import Control.Lens
import Control.Lens.Misc
import qualified Data.JSString as J
import Data.Tagged
import qualified GHC.Generics as G
import Glazier.React
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD

data Footer = Footer
    { activeCount :: Int
    , completedCount :: Int
    , currentFilter :: TD.Filter
    } deriving (Show, Eq, Ord, G.Generic)

makeLenses_ ''Footer

-- hdlSetFilter :: AsReactor cmd => TD.Filter -> Gadget cmd p Footer ()
-- hdlSetFilter fltr = tickScene $  _model._currentFilter .= fltr


-- hdlSetCounts :: AsReactor cmd => (Int, Int) -> Gadget cmd p Footer ()
-- hdlSetCounts (activeCnt, completedCnt) = tickScene $ do
--     _model._activeCount .= activeCnt
--     _model._completedCount .= completedCnt

todoDisplay :: ReactId -> Window Footer ()
todoDisplay ri = do
    s <- ask
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
           then bh' ri "button"
                    [("key", "clear-completed"), ("className", "clear-completed")] $
                    txt "Clear completed"
           else alsoZero

type ClearCompleted = Tagged "ClearCompleted"

todoFooter :: (AsReactor cmd) => ReactId -> Widget cmd p Footer (ClearCompleted ())
todoFooter ri =
    let win = todoDisplay ri
        gad = trigger_ ri _always "onClick" $ Tagged @"ClearCompleted" ()
    in (display win) `also` (lift gad)
