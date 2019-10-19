{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Todo.TodoList where

import qualified Control.Monad.ListM as LM
import qualified GHC.Generics as G
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

-- type TodoCollection = W.DynamicCollection TD.Filter () UKey (Obj TD.Todo)

footer ::
    MonadWidget s m
    => Traversal' s TodoList
    -> m ()
footer this = do
    xs <- maybeM $ (preview $ this._todos) <$> askModel
    let isCompleted obj = completed <$> readObj obj
    (completes, actives) <- LM.partitionM isCompleted xs
    let completedCount = length @[] completes -- LM.partitionM requires inferring
        activeCount = length actives

    bh "footer" [] [("className", "footer")] $ do
        bh "span" [] [("className", "todo-count"), ("key", "todo-count")] $ do
            bh "strong" [] [("key", "items")] $
                txt $ const $ fromString $ show activeCount
            txt " items left"
        bh "ul" [] [("className", "filters"), ("key", "filters")] $ do
            bh "li" [] [("key", "filter-all")] $
                bh "a" [] [ ("href", "#/")
                        , ("key", "all")
                        , ("className", classNames
                            [("selected", preview $ this._filterCriteria.to (== All))])
                        ] $
                txt "All"
            txt " "
            bh "li" [] [("key", "filter-active")] $
                bh "a" [] [ ("href", "#/active")
                        , ("key", "active")
                        , ("className", classNames
                            [("selected", preview $ this._filterCriteria.to (== Active))])
                        ] $
                    txt "Active"
            txt " "
            bh "li" [] [("key", "filter-completed")] $
                bh "a" [] [ ("href", "#/completed")
                        , ("key", "completed")
                        , ("className", classNames
                            [("selected", preview $ this._filterCriteria.to (== Completed))])
                        ] $
                    txt "Completed"
        if (completedCount > 0)
           then bh "button" []
                        [("key", "clear-completed"), ("className", "clear-completed")] $
                    txt "Clear completed"
           else pure ()

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
