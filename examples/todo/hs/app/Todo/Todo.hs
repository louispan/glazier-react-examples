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

import Control.Applicative
import qualified Control.Category as C
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Maybe.Extras as TE
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Generics.Product
import Data.IORef
import qualified Data.JSString as J
import Data.Maybe
import Data.Tagged
import Data.Void
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React.Framework as F
import qualified JavaScript.Extras as JE
import qualified Parameterized.Data.Monoid as P
import qualified Parameterized.TypeLevel as P

data TodoEditRef = TodoEditRef J.JSVal

-- onListingDeleteItem :: (F.MonadReactor x m)
--   => (s -> m CD.Disposable)
--   -> IORef v
--   -> Lens' v (F.ComponentPlan x m, Listing m s s)
--   -> ListingDeleteItem
--   -> m (DL.DList C.Rerender)
-- onListingDeleteItem fin ref this (ListingDeleteItem k) = do
--        F.doModifyIORefM ref $ \obj -> do
--             let mi = M.lookup k (obj ^. this._2.field @"items")
--             fin' <- fromMaybe (pure mempty) (fin <$> mi)
--             pure $ obj & (this._2.field @"items" %~ M.delete k)
--                    . (this._1.field @"disposeOnUpdated" %~ (<> fin'))
--                    . (this._2.field @"list" .~ []) -- this tells render to update displayItems
--        DL.singleton <$> C.mkRerender ref (this._1) (pure mempty)

-- data Command
--     = RenderCommand (F.Gizmo Model Plan) [JE.Property] J.JSVal
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


-- whenTodoEditRef :: (F.MonadReactor x m)
--   => F.Scene x m v TodoModel
--   -> TodoEditRef
--   -> m (DL.DList (Which '[]))
-- whenTodoEditRef (F.Obj ref its) (TodoEditRef j) = do
--        F.doModifyIORef' ref (set' (its._2._1.field @"editRef") j)
--         pure (mempty @(_ (Which '[])))

todoDisplay :: ( F.MonadReactor x m, HasItem' TodoModel ss) => F.FrameDisplay x m ss ()
todoDisplay (cp, ss) = do
    let (p, i) = ss ^. item' @TodoModel
    F.branch "div" []
        [ ("className", JE.classNames [ ("completed", completed i)
                                      , ("editing", editing i)])
        ] $ do
       F.branch "div" [] [ ("key", "view")
                     , ("className", "view")
                     ] $ do
            F.leaf "input"
                (JE.justSnds $ [ ("onChange", onToggleComplete p)
                               ])
                [ ("key", "toggle")
                , ("className", "toggle")
                , ("type", "checkbox")
                , ("checked", JE.toJS' $ completed i)
                ]
            F.branch "label"
                (JE.justSnds $ [ ("onDoubleClick", onStartEdit p)
                               ])
                [ ("key", "label")
                ] (F.txt $ value i)
            F.leaf "button"
                (JE.justSnds $ [ ("onClick", onDestroy p)
                               ])
                [ ("key", "destroy")
                , ("className", "destroy")
                ]
       -- For uncontrolled components, we need to generate a new key per render
       -- in order for react to use the new defaultValue
       F.leaf "input"
           (JE.justSnds $
               [ ("ref", onEditRef p)
               , ("onBlur", onCancelEdit p)
               , ("onKeyDown", onEditKeyDown p)
               ])
           [ ("key", JE.toJS' $ J.unwords
                                       [ F.runReactKey $ F.reactKey cp
                                       , J.pack . show $ F.frameNum cp
                                       ])
           , ("className", "edit")
           , ("defaultValue", JE.toJS' $ value i)
           , ("defaultChecked", JE.toJS' $ completed i)
           ]

data TodoToggleComplete

todoToggleComplete ::
    ( HasItemTag' TodoToggleComplete [F.Listener] s
    , HasItem' TodoInfo s
    , F.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoToggleComplete [F.Listener]])
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
        ( HasItemTag' TodoToggleComplete [F.Listener] s
        , HasItem' TodoInfo s
        , F.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[])
    onChange = F.controlledTrigger' @TodoToggleComplete
            "onChange"
            (const . pure $ DL.singleton ())
            (F.delegate hdlChange)
    hdlChange ::
        (F.MonadReactor x m, HasItem' TodoInfo s)
        => F.SceneHandler x m v s a (Which '[])
    hdlChange = F.Handler $ \this@(F.Obj ref its) _ -> do
        F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"completed" %~ not)
        F.rerender this
        pure zilch

data TodoDestroy = TodoDestroy deriving (G.Generic, NFData)

todoDestroy ::
    ( HasItemTag' TodoDestroy [F.Listener] s
    , F.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoDestroy [F.Listener]])
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
        ( HasItemTag' TodoDestroy [F.Listener] s
        , F.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[TodoDestroy])
    onClick = F.trigger @TodoDestroy
            "onClick"
            (const . pure . DL.singleton . pickOnly $ TodoDestroy)

data TodoLabel

data TodoStartEdit = TodoStartEdit deriving (G.Generic, NFData)

todoLabel ::
    ( HasItemTag' TodoLabel [F.Listener] s
    , HasItem' TodoInfo s
    , F.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoLabel [F.Listener]])
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
        , F.MonadReactor x m
        ) => F.FrameDisplay x m s ()
    dis s =
        (F.txt (s^.F.model.item' @TodoInfo .field @"value"))
    onDoubleClick ::
        ( HasItemTag' TodoLabel [F.Listener] s
        , F.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[TodoStartEdit])
    onDoubleClick = F.trigger @TodoLabel
            "onDoubleClick"
            (const . pure . DL.singleton . pickOnly $ TodoStartEdit)

todoDiv ::
    ( HasItemTag' TodoToggleComplete [F.Listener] s
    , HasItemTag' TodoDestroy [F.Listener] s
    , HasItemTag' TodoLabel [F.Listener] s
    , HasItem' TodoInfo s
    , F.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many
            '[Tagged TodoToggleComplete [F.Listener]
            , Tagged TodoDestroy [F.Listener]
            , Tagged TodoLabel [F.Listener]])
        (Which '[TodoDestroy, TodoStartEdit])
        (Which '[])
        (Which '[])
        (Which '[])
todoDiv = let p = todoToggleComplete
                `F.andPrototype` todoDestroy
                `F.andPrototype` todoLabel
    in p & field @"display" %~ fmap (\a ->
        F.branch "div" []
            [ ("key", "view")
            , ("className", "view")]
            a)

data TodoInput
data TodoCancelEdit = TodoCancelEdit deriving (G.Generic, NFData)

todoInput ::
    ( HasItemTag' TodoInput [F.Listener] s
    , HasItemTag' TodoInput F.EventTarget s
    , HasItem' TodoInfo s
    , F.MonadReactor x m
    )
    => F.Prototype x m v i s
        (Many '[])
        (Many '[Tagged TodoInput [F.Listener], Tagged TodoInput F.EventTarget])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
        (Which '[])
todoInput = F.widget @TodoInput "input"
    (\s ->
        -- For uncontrolled components, we need to generate a new key per render
        -- in order for react to use the new defaultValue
        [ ("key", JE.toJS' $ J.unwords
            [ F.runReactKey . F.reactKey $ s ^. F.plan
            , J.pack . show . F.frameNum $ s ^. F.plan
            ])
        , ("className", "edit")
        , ("defaultValue", JE.toJS' . value $ s ^. F.model.item')
        , ("defaultChecked", JE.toJS' . completed $ s ^. F.model.item')])
    (F.withRef @TodoInput `F.andPrototype` F.nilPrototype
        { F.activator = onBlur `F.andExActivator` onKeyDown })
  where
    onBlur ::
        ( HasItemTag' TodoInput [F.Listener] s
        , HasItem' TodoInfo s
        , F.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[])
    onBlur = F.controlledTrigger' @TodoInput
            "onBlur"
            (const . pure $ DL.singleton $ TodoCancelEdit)
            (F.delegate hdlBlur)
    hdlBlur ::
        (F.MonadReactor x m, HasItem' TodoInfo s)
        => F.SceneHandler x m v s TodoCancelEdit (Which '[])
    hdlBlur = F.Handler $ \this@(F.Obj ref its) _ -> do
        F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"completed" %~ not)
        F.rerender this
        pure zilch
    onKeyDown ::
        ( HasItemTag' TodoInput [F.Listener] s
        , HasItem' TodoInfo s
        , F.MonadReactor x m
        ) => F.ProtoActivator x m v s (Which '[TodoDestroy])
    onKeyDown = F.controlledTrigger' @TodoInput
            "onKeyDown"
            (TE.fromMaybeT . F.fireKeyDownKey)
            (F.delegate hdlKeyDown)
    hdlKeyDown ::
        (F.MonadReactor x m, HasItem' TodoInfo s)
        => F.SceneHandler x m v s F.KeyDownKey (Which '[TodoDestroy])
    hdlKeyDown = F.Handler $ \this@(F.Obj ref its) (F.KeyDownKey j k) ->
        case k of
            "Enter" -> do
                v <- JE.fromJS' <$> F.doGetProperty "value" (JE.toJS j)
                let v' = J.strip $ fromMaybe J.empty v
                if J.null v'
                    then
                        pure . DL.singleton . pickOnly $ TodoDestroy
                    else do
                        F.doModifyIORef' ref (\s -> s
                            & its.F.model.item' @TodoInfo .field @"editing" .~ False
                            & its.F.model.item' @TodoInfo .field @"value" .~ v'
                            )
                        F.rerender this
                        F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j)
                        pure mempty
            "Escape" -> do
                F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"editing" .~ False)
                F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j)
                F.rerender this
                pure mempty
            _ -> pure mempty
    hdlStartEdit ::
        ( F.MonadReactor x m
        , HasItem' TodoInfo s
        , HasItemTag' TodoInput F.EventTarget s)
        => F.SceneHandler x m v s TodoStartEdit (Which '[])
    hdlStartEdit = F.Handler $ \this@(F.Obj ref its) _ -> do
        void $ runMaybeT $ do
            obj <- lift $ F.doReadIORef ref
            let b = obj ^. its.F.model.item' @TodoInfo .field @"completed"
            guard (not b)
            -- Focus after rendering changed because we are using uncontrollec components
            -- with a new key. This will result in a different input element after each render
            lift $ F.doModifyIORef' ref (\s -> s
                & its.F.model.item' @TodoInfo .field @"editing" .~ True
                & its.F.plan %~ (F.scheduleAfterOnUpdated $ F.focus @TodoInput this)
                )
            lift $ F.rerender this
        pure zilch


-- data GetProperty x m = GetProperty J.JSString J.JSVal (JE.JSVar -> m ())

-- wock ::
--     ( HasItemTag' TodoCheckbox [F.Listener] s
--     , HasItem' TodoInfo s
--     , F.MonadReactor x m
--     ) => F.ProtoActivator x m v s (Which '[C.Rerender])
-- wock = F.withExecutor pickOnly wack

-- wick :: ( HasItemTag' TodoCheckbox [F.Listener] s
--     , HasItem' TodoInfo s
--     , F.MonadReactor x m
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

-- wick2 :: ( HasItemTag' TodoCheckbox [F.Listener] s
--     , HasItem' TodoInfo s
--     , F.MonadReactor x m
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
--     ( HasItemTag' TodoCheckbox [F.Listener] s
--     , HasItem' TodoInfo s
--     , F.MonadReactor x m
--     ) => F.ExObjActivator m v (F.ComponentPlan x m, s) x (Which '[C.Rerender])

-- onCancelEdit' :: J.JSVal -> MaybeT IO [Action]
-- onCancelEdit' = F.eventHandlerM F.Input.whenBlur goLazy
--   where
--     goLazy :: J.JSVal -> MaybeT IO [Action]
--     goLazy j = pure [CancelEditAction j]

-- onEditKeyDown' :: J.JSVal -> MaybeT IO [Action]
-- onEditKeyDown' = F.eventHandlerM F.Input.whenKeyDown goLazy
--   where
--     goLazy :: (J.JSVal, Maybe J.JSString) -> MaybeT IO [Action]
--     goLazy (j, ms) = pure $
--         -- We have finished with the edit input form, reset the input value to keep the DOM clean.
--         maybe [CancelEditAction j] (pure . SubmitAction j) ms

-- gadget :: G.Gadget Action (F.Gizmo Model Plan) (D.DList Command)
-- gadget = do
--     a <- ask
--     case a
--         -- common widget actions
--           of
--         ComponentRefAction node -> do
--             componentRef .= node
--             pure mempty
--         RenderAction ->
--             D.singleton <$> F.basicRenderCmd frameNum componentRef RenderCommand
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
--             D.singleton <$> F.basicRenderCmd frameNum componentRef RenderCommand
--         SetCompletedAction b -> do
--             completed .= b
--             D.singleton <$> F.basicRenderCmd frameNum componentRef RenderCommand
--         StartEditAction -> do
--             ret <-
--                 runMaybeT $ do
--                     b <- use completed
--                     guard (not b)
--                     editing .= True
--                 -- Need to delay focusing until after the next render
--                     autoFocusEdit .= True
--                     D.singleton <$>
--                         F.basicRenderCmd frameNum componentRef RenderCommand
--             maybe (pure mempty) pure ret
--         -- parent widgets should detect this case to do something with submitted action
--         DestroyAction -> pure mempty
--         CancelEditAction j -> do
--             editing .= False
--             cmd <- F.basicRenderCmd frameNum componentRef RenderCommand
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
--                       else F.basicRenderCmd frameNum componentRef RenderCommand
--             pure $ D.fromList
--                     [ SetPropertyCommand ("value", JE.toJS' J.empty) j
--                     , cmd
--                     ]
