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

module Todo.Todo where

import Control.DeepSeq
import Control.Lens
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Generics.Product
import Data.IORef
import qualified Data.JSString as J
import Data.Tagged
import Data.Void
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified Glazier.React.Widget as W
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
  => F.Scene x m v TodoModel
  -> TodoEditRef
  -> m (DL.DList Void)
whenTodoEditRef (F.Obj ref its) (TodoEditRef j) = do
       R.doModifyIORef' ref (set' (its._2._1.field @"editRef") j)
       pure mempty

todoDisplay :: ( R.MonadReactor x m, HasItem' TodoModel ss) => F.FrameDisplay x m ss ()
todoDisplay (cp, ss) = do
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

data TodoToggleComplete

todoToggleComplete ::
    ( HasItemTag' TodoToggleComplete [R.Listener] s
    , HasItem' TodoInfo s
    , R.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoToggleComplete [R.Listener]])
        (Which '[])
        (Which '[])
        (Which '[])
        (Which '[])
todoToggleComplete = F.widget @TodoToggleComplete "input"
    (\s ->
        [ ("key", "toggle")
        , ("className", "toggle")
        , ("type", "checkbox")
        , ("checked", JE.toJS' . completed $ s ^. F.model.item')])
    (F.nilPrototype { F.activator = onChange })
  where
    onChange ::
        ( HasItemTag' TodoToggleComplete [R.Listener] s
        , HasItem' TodoInfo s
        , R.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[])
    onChange = F.withExecutor (absurd @(Which '[])) $ F.controlledTrigger' @TodoToggleComplete
            "onChange"
            (const . pure $ DL.singleton ())
            (F.delegate hdlChange)
    hdlChange ::
        (R.MonadReactor x m, HasItem' TodoInfo s)
        => F.SceneHandler x m v s a Void
    hdlChange = F.Handler $ \this@(F.Obj ref its) _ -> do
        R.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"completed" %~ not)
        F.rerender this
        pure mempty

data TodoDestroy = TodoDestroy deriving (G.Generic, NFData)

todoDestroy ::
    ( HasItemTag' TodoDestroy [R.Listener] s
    , R.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoDestroy [R.Listener]])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
        (Which '[])
todoDestroy = F.widget @TodoDestroy "button"
    (const
        [ ("key", "destroy")
        , ("className", "destroy")])
    (F.nilPrototype { F.activator = onClick })
  where
    onClick ::
        ( HasItemTag' TodoDestroy [R.Listener] s
        , R.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[TodoDestroy])
    onClick = F.trigger @TodoDestroy
            "onClick"
            (const . pure . DL.singleton . pickOnly $ TodoDestroy)

data TodoLabel

data TodoStartEdit = TodoStartEdit deriving (G.Generic, NFData)

todoLabel ::
    ( HasItemTag' TodoLabel [R.Listener] s
    , HasItem' TodoInfo s
    , R.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoLabel [R.Listener]])
        (Which '[TodoStartEdit])
        (Which '[])
        (Which '[])
        (Which '[])
todoLabel = F.widget @TodoLabel "label"
    (const [ ("key", "label")])
    (F.nilPrototype
        { F.display = dis
        , F.activator = onDoubleClick
        })
  where
    dis ::
        ( HasItem' TodoInfo s
        , R.MonadReactor x m
        ) => F.FrameDisplay x m s ()
    dis s =
        (R.txt (s^.F.model.item' @TodoInfo .field @"value"))
    onDoubleClick ::
        ( HasItemTag' TodoLabel [R.Listener] s
        , R.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[TodoStartEdit])
    onDoubleClick = F.trigger @TodoLabel
            "onDoubleClick"
            (const . pure . DL.singleton . pickOnly $ TodoStartEdit)

todoDiv ::
    ( HasItemTag' TodoToggleComplete [R.Listener] s
    , HasItemTag' TodoDestroy [R.Listener] s
    , HasItemTag' TodoLabel [R.Listener] s
    , HasItem' TodoInfo s
    , R.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many
            '[Tagged TodoToggleComplete [R.Listener]
            , Tagged TodoDestroy [R.Listener]
            , Tagged TodoLabel [R.Listener]])
        (Which '[TodoDestroy, TodoStartEdit])
        (Which '[])
        (Which '[])
        (Which '[])
todoDiv = let p = todoToggleComplete
                `P.pmappend` todoDestroy
                `P.pmappend` todoLabel
    in p & field @"display" %~ fmap (\x ->
        R.branch "div" []
            [ ("key", "view")
            , ("className", "view")]
            x)

data TodoInput
data TodoCancelEdit = TodoCancelEdit deriving (G.Generic, NFData)

todoInput ::
    ( HasItemTag' TodoInput [R.Listener] s
    , HasItemTag' TodoInput R.EventTarget s
    , HasItem' TodoInfo s
    , R.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoInput [R.Listener], Tagged TodoInput R.EventTarget])
        (Which '[])
        (Which '[])
        (Which '[TodoCancelEdit])
        (Which '[])
todoInput = F.widget @TodoInput "input"
    (\s ->
        -- For uncontrolled components, we need to generate a new key per render
        -- in order for react to use the new defaultValue
        [ ("key", JE.toJS' $ J.unwords
            [ R.runReactKey . F.key $ s ^. F.plan
            , J.pack . show . F.frameNum $ s ^. F.plan
            ])
        , ("className", "edit")
        , ("defaultValue", JE.toJS' . value $ s ^. F.model.item')
        , ("defaultChecked", JE.toJS' . completed $ s ^. F.model.item')])
    (W.withRef @TodoInput `F.andPrototype` F.nilPrototype
        { F.activator = onBlur
        , F.handler = F.delegate hdlBlur })
  where
    onBlur ::
        ( HasItemTag' TodoInput [R.Listener] s
        , HasItem' TodoInfo s
        , R.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[])
    onBlur = F.controlledTrigger' @TodoInput
            "onBlur"
            (const . pure $ DL.singleton . pickOnly $ TodoCancelEdit)
            (F.delegate hdlBlur)
    hdlBlur ::
        (R.MonadReactor x m, HasItem' TodoInfo s)
        => F.SceneHandler x m v s (Which '[TodoCancelEdit]) (Which '[])
    hdlBlur = F.Handler $ \this@(F.Obj ref its) _ -> do
        R.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"completed" %~ not)
        F.rerender this
        pure mempty



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
