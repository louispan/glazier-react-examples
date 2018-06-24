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
    ( TodoCollection
    , todoFooter
    ) where

import Control.Lens
import qualified Control.Monad.ListM as LM
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Glazier.React
import Glazier.React.Effect.JavaScript
import qualified Glazier.React.Widgets.Collection.Dynamic as W
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD
import qualified Todo.Todo as TD

type TodoCollection f = W.DynamicCollection TD.Filter () W.UKey TD.Todo f

todoDisplay :: ReactId -> Window (TodoCollection Subject) ()
todoDisplay ri = do
    s <- ask
    let xs = s ^. (_model.W._rawCollection.to toList)
        isActive sbj = do
            td <- doReadIORef (sceneRef sbj)
            pure $ td ^. _model.TD._completed
    (completed, active) <- lift $ LM.partitionM isActive xs
    let completedCount = length @[] completed
        activeCount = length active
    bh "footer" [("key", JE.toJSR $ ri), ("className", "footer")] $ do
        bh "span" [ ("className", "todo-count")
                    , ("key", "todo-count")] $ do
            bh "strong" [("key", "items")] (txt $ J.pack $ show activeCount)
            txt " items left"
        bh "ul" [("className", "filters")
                  , ("key", "filters")] $ do
            bh "li" [("key", "filter-all")] $
                bh "a" [ ("href", "#/")
                         , ("key", "all")
                         , ("className", JE.classNames
                            [("selected"
                            , s ^. _model.W._filterCriteria == TD.All)])
                         ] $
                txt "All"
            txt " "
            bh "li" [("key", "filter-active")] $
                bh "a"
                [ ("href", "#/active")
                , ("key", "active")
                , ("className", JE.classNames
                    [("selected"
                    , s ^. _model.W._filterCriteria == TD.Active)])
                ] $
                txt "Active"
            txt " "
            bh "li" [("key", "filter-completed")] $
                bh "a"
                    [ ("href", "#/completed")
                    , ("key", "completed")
                    , ("className", JE.classNames
                        [("selected"
                        , s ^. _model.W._filterCriteria == TD.Completed)])
                    ] $
                    txt "Completed"
        if (completedCount > 0)
           then bh' ri "button"
                    [("key", "clear-completed"), ("className", "clear-completed")] $
                    txt "Clear completed"
           else alsoZero

-- | The 'JE.JSRep' arg should be @document.defaultView@ or @window@
todoFooter :: (AsReactor cmd, AsJavascript cmd)
    => JE.JSRep -> ReactId -> Widget cmd p (TodoCollection Subject) r
todoFooter j ri =
    let win = todoDisplay ri
        gad = (finish $ hdlHashChange j)
            `also` (finish $ hdlClearCompleted ri)
            `also` (finish (onMounted (hdlMounted j)))
    in (display win) `also` (lift gad)
  where

hdlClearCompleted :: (AsReactor cmd) => ReactId -> Gadget cmd p (TodoCollection Subject) ()
hdlClearCompleted ri = do
    trigger_ ri "onClick" ()
    tickModel $ do
        xs <- use (W._rawCollection.to M.toList)
        xs' <- lift $ LM.filterMP (ftr . snd) xs
        W._rawCollection .= (M.fromList xs')
        ys <- use (W._visibleList)
        ys' <- lift $ LM.filterMP ftr ys
        W._visibleList .= ys'
  where
    ftr x = do
        x' <- doReadIORef $ sceneRef x
        pure $ x' ^. _model.TD._completed.to not

hdlHashChange :: (AsReactor cmd) => JE.JSRep -> Gadget cmd p (TodoCollection Subject) ()
hdlHashChange j = do
    ftr <- mapHashChange <$> domTrigger j "hashchange" whenHashChange
    tickModel $ W._filterCriteria .= ftr

hdlMounted ::
    ( AsReactor cmd
    , AsJavascript cmd
    )
    => JE.JSRep -> Gadget cmd p (TodoCollection Subject) ()
hdlMounted j = do
    (`evalMaybeT` ()) $ do
        w <- lift $ sequel $ postCmd' . GetProperty j "location"
        h <- MaybeT . fmap JE.fromJSR . sequel $ postCmd' . GetProperty w "hash"
        let ftr = mapHashChange h
        tickModel $ W._filterCriteria .= ftr

-- | Provide split up parts of onHashChange in case the applications
-- needs to combine other widgets that also uses hashchange event
whenHashChange :: JE.JSRep -> MaybeT IO J.JSString
whenHashChange evt = do
    newURL <- MaybeT (JE.fromJSR <$> JE.getProperty evt "newURL")
    let (_, newHash) = J.breakOn "#" newURL
    pure newHash

-- | Provide split up parts of onHashChange in case the applications
-- needs to combine other widgets that also uses hashchange event
mapHashChange :: J.JSString -> TD.Filter
mapHashChange newHash =
    case newHash of
        "#/active" -> TD.Active
        "#/completed" -> TD.Completed
        _ -> TD.All
