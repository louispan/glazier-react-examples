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

module Todo.TodoList where

import Control.Concurrent.MVar
-- import Control.Lens
-- import qualified Control.Monad.ListM as LM
-- import Data.Foldable
-- import qualified Data.JSString as J
import Glazier.React
import qualified Todo.Todo as TD

default (JSString)

data Filter = All | Active | Completed
    deriving (Eq, Show, Ord, G.Generic)

-- type TodoCollection = W.DynamicCollection TD.Filter () UKey (Obj TD.Todo)

footer ::
    MonadWidget s c m
    => Traversal' s J.JSString
    -> m ()
footer this = do
    xs <- askModel
    let isActive obj = TD.completed <$> liftIO $ readMVar $ modelVar obj
    (completed, active) <- lift $ LM.partitionM isActive xs
    let completedCount = length completed
        activeCount = length active

    bh' "footer" [("className", strProp' "footer")] $ do
        bh' "span" [ ("className", "todo-count")
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
                            , s ^. _meta.W._filterCriteria == TD.All)])
                         ] $
                txt "All"
            txt " "
            bh "li" [("key", "filter-active")] $
                bh "a"
                [ ("href", "#/active")
                , ("key", "active")
                , ("className", JE.classNames
                    [("selected"
                    , s ^. _meta.W._filterCriteria == TD.Active)])
                ] $
                txt "Active"
            txt " "
            bh "li" [("key", "filter-completed")] $
                bh "a"
                    [ ("href", "#/completed")
                    , ("key", "completed")
                    , ("className", JE.classNames
                        [("selected"
                        , s ^. _meta.W._filterCriteria == TD.Completed)])
                    ] $
                    txt "Completed"
        if (completedCount > 0)
           then bh' k "button"
                    [("key", "clear-completed"), ("className", "clear-completed")] $
                    txt "Clear completed"
           else alsoZero

-- -- | The 'JE.JSRep' arg should be @document.defaultView@ or @window@
-- todoCollection :: (AsReactor c, AsJavascript c)
--     => JE.JSRep -> ReactId -> Widget c o TodoCollection r
-- todoCollection j k =
--     let win = foooter k
--         gad = (finish $ hdlHashChange k j)
--             `also` (finish $ hdlClearCompleted k)
--             `also` (finish $ hdlMounted k j)
--     in (display win) `also` (lift gad)

-- hdlClearCompleted :: (AsReactor c) => ReactId -> Gadget c o TodoCollection ()
-- hdlClearCompleted k = do
--     trigger_ k "onClick" ()
--     mutate k $ do
--         xs <- use (W._rawCollection.to M.toList)
--         xs' <- lift $ LM.filterMP (ftr . snd) xs
--         W._rawCollection .= (M.fromList xs')
--         ys <- use (W._visibleList)
--         ys' <- lift $ LM.filterMP ftr ys
--         W._visibleList .= ys'
--   where
--     ftr x = do
--         x' <- benignReadIORef $ sceneRef x
--         pure $ x' ^. _meta.TD._completed.to not

-- hdlHashChange :: (AsReactor c) => ReactId -> JE.JSRep -> Gadget c o TodoCollection ()
-- hdlHashChange k j = do
--     ftr <- mapHashChange <$> domTrigger j "hashchange" whenHashChange
--     mutate k $ W._filterCriteria .= ftr

-- -- | The 'JE.JSRep' arg should be @document.defaultView@ or @window@
-- hdlMounted ::
--     ( AsReactor c
--     , AsJavascript c
--     )
--     => ReactId -> JE.JSRep -> Gadget c o TodoCollection ()
-- hdlMounted k j = onMounted $ do
--     (`evalMaybeT` ()) $ do
--         h <- MaybeT $ JE.fromJSR <$> (getProperty "location" j
--              >>= getProperty "hash")
--         let ftr = mapHashChange h
--         mutate k $ W._filterCriteria .= ftr

-- -- | Provide split up parts of onHashChange in case the applications
-- -- needs to combine other widgets that also uses hashchange event
-- whenHashChange :: JE.JSRep -> MaybeT IO J.JSString
-- whenHashChange evt = do
--     newURL <- MaybeT (JE.fromJSR <$> JE.getPropertyIO "newURL" evt)
--     let (_, newHash) = J.breakOn "#" newURL
--     pure newHash

-- -- | Provide split up parts of onHashChange in case the applications
-- -- needs to combine other widgets that also uses hashchange event
-- mapHashChange :: J.JSString -> TD.Filter
-- mapHashChange newHash =
--     case newHash of
--         "#/active" -> TD.Active
--         "#/completed" -> TD.Completed
--         _ -> TD.All
