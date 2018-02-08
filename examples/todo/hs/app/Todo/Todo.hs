{-# LANGUAGE DataKinds #-}
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

module Todo.Todo where

import Control.Lens
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Generics.Product
import Data.IORef
import qualified Data.JSString as J
import Data.Tagged
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Commands as C
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Prototypes as W
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

data TodoEditRef = TodoEditRef J.JSVal

-- onListingDeleteItem :: (R.MonadReactor x m)
--   => (s -> m CD.Disposable)
--   -> IORef v
--   -> Lens' v (F.ComponentPlan x m, Listing m s s)
--   -> ListingDeleteItem
--   -> m (DL.DList C.Rerender)
-- onListingDeleteItem fin ref this (ListingDeleteItem k) = do
--        R.doModifyIORefM ref $ \obj -> do
--             let mi = M.lookup k (obj ^. this._2.field @"items")
--             fin' <- fromMaybe (pure mempty) (fin <$> mi)
--             pure $ obj & (this._2.field @"items" %~ M.delete k)
--                    . (this._1.field @"disposeOnUpdated" %~ (<> fin'))
--                    . (this._2.field @"list" .~ []) -- this tells render to update displayItems
--        DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

-- data Command
--     = RenderCommand (R.Gizmo Model Plan) [JE.Property] J.JSVal
--     | SetPropertyCommand JE.Property J.JSVal
--     | FocusNodeCommand J.JSVal
--     | SendDestroyActionCommand

-- data Action
--     = ComponentRefAction J.JSVal
--     | RenderAction
--     | ComponentDidUpdateAction
--     | SetPropertyAction JE.Property J.JSVal
--     | EditRefAction J.JSVal
--     | StartEditAction
--     | ToggleCompletedAction
--     | SetCompletedAction Bool
--     | DestroyAction
--     | CancelEditAction J.JSVal
--     | SubmitAction J.JSVal J.JSString

data TodoInfo = TodoInfo
    { value :: J.JSString
    , completed :: Bool
    , editing :: Bool
    , autoFocusEdit :: Bool
    } deriving G.Generic

data TodoPlan = TodoPlan
    { editRef :: J.JSVal
    , onToggleComplete :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onStartEdit :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onDestroy :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onCancelEdit :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onEditKeyDown :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onEditRef :: Maybe (J.Callback (J.JSVal -> IO ()))
    } deriving G.Generic

type TodoModel = (TodoPlan, TodoInfo)


whenTodoEditRef :: (R.MonadReactor x m)
  => IORef v
  -> Lens' v (F.ComponentPlan x m, TodoModel)
  -> TodoEditRef
  -> m (DL.DList c)
whenTodoEditRef ref this (TodoEditRef j) = do
       R.doModifyIORef' ref (set' (this._2._1.field @"editRef") j)
       pure mempty

todoDisplay :: ( R.MonadReactor x m, HasItem' TodoModel ss) => F.Display m (F.ComponentPlan x m, ss) ()
todoDisplay = F.Display $ \(cp, ss) -> do
    let (p, i) = ss ^. item' @TodoModel
    R.branch "div" []
        [ ("className", JE.classNames [ ("completed", completed i)
                                      , ("editing", editing i)])
        ] $ do
       R.branch "div" [] [ ("key", "view")
                     , ("className", "view")
                     ] $ do
            R.leaf "input"
                (JE.justSnds $ [ ("onChange", onToggleComplete p)
                               ])
                [ ("key", "toggle")
                , ("className", "toggle")
                , ("type", "checkbox")
                , ("checked", JE.toJS' $ completed i)
                ]
            R.branch "label"
                (JE.justSnds $ [ ("onDoubleClick", onStartEdit p)
                               ])
                [ ("key", "label")
                ] (R.txt $ value i)
            R.leaf "button"
                (JE.justSnds $ [ ("onClick", onDestroy p)
                               ])
                [ ("key", "destroy")
                , ("className", "destroy")
                ]
       -- For uncontrolled components, we need to generate a new key per render
       -- in order for react to use the new defaultValue
       R.leaf "input"
           (JE.justSnds $
               [ ("ref", onEditRef p)
               , ("onBlur", onCancelEdit p)
               , ("onKeyDown", onEditKeyDown p)
               ])
           [ ("key", JE.toJS' $ J.unwords
                                       [ R.runReactKey $ F.key cp
                                       , J.pack . show $ F.frameNum cp
                                       ])
           , ("className", "edit")
           , ("defaultValue", JE.toJS' $ value i)
           , ("defaultChecked", JE.toJS' $ completed i)
           ]

data TodoCheckbox

checkbox ::
    ( HasItemTag' TodoCheckbox [R.Listener] s
    , HasItem' TodoInfo s
    , R.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoCheckbox [R.Listener]])
        (Which '[C.Rerender])
        (Which '[])
        (Which '[])
        (Which '[])
checkbox = F.widget @TodoCheckbox "input"
    (\s ->
        [ ("key", "toggle")
        , ("className", "toggle")
        , ("type", "checkbox")
        , ("checked", JE.toJS' . completed $ view item' s)])
    (P.pmempty { F.activator = onChange })
  where
    onChange ::
        ( HasItemTag' TodoCheckbox [R.Listener] s
        , HasItem' TodoInfo s
        , R.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[C.Rerender])
    onChange = F.withExecutor pickOnly $ F.controlledTrigger' @TodoCheckbox
            "onChange"
            (const . pure $ DL.singleton ())
            (F.delegate (F.Handler whenChange))
    whenChange ::
        (R.MonadReactor x m, HasItem' TodoInfo s)
        => F.ComObject x m v s
        -> ()
        -> m (DL.DList C.Rerender)
    whenChange (F.Object ref (Lens this)) _ = do
            R.doModifyIORef' ref (this._2.item' @TodoInfo .field @"completed" %~ not)
            C.mkRerender' ref this

-- wock ::
--     ( HasItemTag' TodoCheckbox [R.Listener] s
--     , HasItem' TodoInfo s
--     , R.MonadReactor x m
--     ) => F.ProtoActivator x m v s (Which '[C.Rerender])
-- wock = F.withExecutor pickOnly wack

-- wick :: ( HasItemTag' TodoCheckbox [R.Listener] s
--     , HasItem' TodoInfo s
--     , R.MonadReactor x m
--     ) => F.Prototype.Prototype
--                        x
--                        m
--                        v
--                        i
--                        s
--                        (Many '[])
--                        (Many '[])
--                        (Which '[C.Rerender])
--                        (Which '[])
--                        (Which '[])
--                        (Which '[])
-- wick = F.Prototype
--     P.pmempty
--     mempty
--     mempty
--     wock
--     P.pmempty

-- wick2 :: ( HasItemTag' TodoCheckbox [R.Listener] s
--     , HasItem' TodoInfo s
--     , R.MonadReactor x m
--     ) => F.Prototype.Prototype
--                        x
--                        m
--                        v
--                        i
--                        s
--                        (Many '[])
--                        (Many '[])
--                        (Which '[C.Rerender])
--                        (Which '[])
--                        (Which '[])
--                        (Which '[])
-- wick2 = P.pmempty { F.Prototype.activator = wock }

-- wock ::
--     ( HasItemTag' TodoCheckbox [R.Listener] s
--     , HasItem' TodoInfo s
--     , R.MonadReactor x m
--     ) => F.ExObjActivator m v (F.ComponentPlan x m, s) x (Which '[C.Rerender])

-- onCancelEdit' :: J.JSVal -> MaybeT IO [Action]
-- onCancelEdit' = R.eventHandlerM W.Input.whenBlur goLazy
--   where
--     goLazy :: J.JSVal -> MaybeT IO [Action]
--     goLazy j = pure [CancelEditAction j]

-- onEditKeyDown' :: J.JSVal -> MaybeT IO [Action]
-- onEditKeyDown' = R.eventHandlerM W.Input.whenKeyDown goLazy
--   where
--     goLazy :: (J.JSVal, Maybe J.JSString) -> MaybeT IO [Action]
--     goLazy (j, ms) = pure $
--         -- We have finished with the edit input form, reset the input value to keep the DOM clean.
--         maybe [CancelEditAction j] (pure . SubmitAction j) ms

-- gadget :: G.Gadget Action (R.Gizmo Model Plan) (D.DList Command)
-- gadget = do
--     a <- ask
--     case a
--         -- common widget actions
--           of
--         ComponentRefAction node -> do
--             componentRef .= node
--             pure mempty
--         RenderAction ->
--             D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
--         ComponentDidUpdateAction
--             -- Run delayed action that need to wait until frame is re-rendered
--             -- Eg focusing after other rendering changes
--          -> do
--             focus' <- use autoFocusEdit
--             autoFocusEdit .= False
--             -- Focus after rendering changed because a new input element might have been rendered
--             ret <-
--                 runMaybeT $ do
--                     guard focus'
--                     input <- use editRef
--                     input' <- MaybeT $ pure $ JE.fromJS input
--                     pure $ D.singleton $ FocusNodeCommand input'
--             maybe (pure mempty) pure ret
--         SetPropertyAction props j ->
--             pure $ D.singleton $ SetPropertyCommand props j
--         EditRefAction v -> do
--             editRef .= v
--             pure mempty
--         ToggleCompletedAction -> do
--             completed %= not
--             D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
--         SetCompletedAction b -> do
--             completed .= b
--             D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
--         StartEditAction -> do
--             ret <-
--                 runMaybeT $ do
--                     b <- use completed
--                     guard (not b)
--                     editing .= True
--                 -- Need to delay focusing until after the next render
--                     autoFocusEdit .= True
--                     D.singleton <$>
--                         R.basicRenderCmd frameNum componentRef RenderCommand
--             maybe (pure mempty) pure ret
--         -- parent widgets should detect this case to do something with submitted action
--         DestroyAction -> pure mempty
--         CancelEditAction j -> do
--             editing .= False
--             cmd <- R.basicRenderCmd frameNum componentRef RenderCommand
--             pure $ D.fromList
--                     [ SetPropertyCommand ("value", JE.toJS' J.empty) j
--                     , cmd
--                     ]
--         SubmitAction j v
--             -- trim the text
--          -> do
--             let v' = J.strip v
--             value .= v'
--             editing .= False
--             cmd <- if J.null v'
--                       then pure SendDestroyActionCommand
--                       else R.basicRenderCmd frameNum componentRef RenderCommand
--             pure $ D.fromList
--                     [ SetPropertyCommand ("value", JE.toJS' J.empty) j
--                     , cmd
--                     ]
