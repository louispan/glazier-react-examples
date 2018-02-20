{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
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

-- onListingDeleteItem :: (F.MonadReactor m)
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


-- whenTodoEditRef :: (F.MonadReactor m)
--   => F.Scene x m v TodoModel
--   -> TodoEditRef
--   -> m (DL.DList (Which '[]))
-- whenTodoEditRef (F.Obj ref its) (TodoEditRef j) = do
--        F.doModifyIORef' ref (set' (its._2._1.field @"editRef") j)
--         pure (mempty @(_ (Which '[])))

todoDisplay :: ( F.MonadReactor m, HasItem' TodoModel ss) => F.FrameDisplay m ss ()
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
    ( F.MonadReactor m
    , HasItemTag' TodoToggleComplete [F.Listener] s
    , HasItem' TodoInfo s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged TodoToggleComplete [F.Listener]])
        (Which '[])
        (Which '[])
        (Which '[])
todoToggleComplete = F.widget @TodoToggleComplete "input"
    (\s ->
        [ ("key", "toggle")
        , ("className", "toggle")
        , ("type", "checkbox")
        , ("checked", JE.toJS' . completed $ s ^. F.model.item')])
    (F.nulPrototype { F.activator' = onChange })
  where
    onChange ::
        ( HasItemTag' TodoToggleComplete [F.Listener] s
        , HasItem' TodoInfo s
        , F.MonadReactor m
        ) => F.SceneActivator m v s (Which '[])
    onChange = F.controlledTrigger @TodoToggleComplete
            "onChange"
            (const $ pure ())
            hdlChange

    hdlChange ::
        (F.MonadReactor m, HasItem' TodoInfo s)
        => F.SceneHandler m v s () (Which '[])
    hdlChange this@(F.Obj ref its) _ = F.terminate' . lift $ do
        F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"completed" %~ not)
        F.rerender' this

data TodoDestroy = TodoDestroy deriving (G.Generic, NFData)
todoDestroy ::
    ( F.MonadReactor m
    , HasItemTag' TodoDestroy [F.Listener] s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged TodoDestroy [F.Listener]])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
todoDestroy = F.widget @TodoDestroy "button"
    (const
        [ ("key", "destroy")
        , ("className", "destroy")])
    (F.nulPrototype { F.activator' = onClick })
  where
    onClick ::
        ( HasItemTag' TodoDestroy [F.Listener] s
        , F.MonadReactor m
        ) => F.SceneActivator m v s (Which '[TodoDestroy])
    onClick = F.trigger @TodoDestroy
            "onClick"
            (const . pure $ pickOnly TodoDestroy)

data TodoLabel
data TodoStartEdit = TodoStartEdit deriving (G.Generic, NFData)

todoLabel ::
    ( F.MonadReactor m
    , HasItemTag' TodoLabel [F.Listener] s
    , HasItem' TodoInfo s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged TodoLabel [F.Listener]])
        (Which '[TodoStartEdit])
        (Which '[])
        (Which '[])
todoLabel = F.widget @TodoLabel "label"
    (const [ ("key", "label")])
    (F.nulPrototype
        { F.display' = dis
        , F.activator' = onDoubleClick
        })
  where
    dis ::
        ( HasItem' TodoInfo s
        , F.MonadReactor m
        ) => F.FrameDisplay m s ()
    dis s =
        (F.txt (s^.F.model.item' @TodoInfo .field @"value"))

    onDoubleClick ::
        ( HasItemTag' TodoLabel [F.Listener] s
        , F.MonadReactor m
        ) => F.SceneActivator m v s (Which '[TodoStartEdit])
    onDoubleClick = F.trigger @TodoLabel
            "onDoubleClick"
            (const . pure $ pickOnly TodoStartEdit)

todoView ::
    ( F.MonadReactor m
    , HasItemTag' TodoToggleComplete [F.Listener] s
    , HasItemTag' TodoDestroy [F.Listener] s
    , HasItemTag' TodoLabel [F.Listener] s
    , HasItem' TodoInfo s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many
            '[Tagged TodoToggleComplete [F.Listener]
            , Tagged TodoDestroy [F.Listener]
            , Tagged TodoLabel [F.Listener]])
        (Which '[TodoDestroy, TodoStartEdit])
        (Which '[])
        (Which '[])
todoView =
    let p = todoToggleComplete
                    `F.andPrototype` todoDestroy
                    `F.andPrototype` todoLabel
        disp = F.display' p
    in p { F.display' = \s -> F.branch "div" []
                [ ("key", "view")
                , ("className", "view")]
                (disp s) }

data TodoInput
data TodoCancelEdit = TodoCancelEdit deriving (G.Generic, NFData)

todoInput ::
    ( F.MonadReactor m
    , F.MonadJS m
    , F.MonadHTMLElement m
    , HasItemTag' TodoInput [F.Listener] s
    , HasItemTag' TodoInput F.EventTarget s
    , HasItem' TodoInfo s
    )
    => F.Prototype m v i s
        (Many '[])
        (Many '[Tagged TodoInput [F.Listener], Tagged TodoInput F.EventTarget])
        (Which '[TodoDestroy])
        (Which '[TodoStartEdit])
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
    (F.withRef @TodoInput `F.andPrototype` F.nulPrototype
        { F.activator' = onBlur `F.andActivator` onKeyDown
        , F.handler' = (. obvious) <$> hdlStartEdit })
  where
    onBlur ::
        ( HasItemTag' TodoInput [F.Listener] s
        , HasItem' TodoInfo s
        , F.MonadReactor m
        ) => F.SceneActivator m v s (Which '[])
    onBlur = F.controlledTrigger @TodoInput
            "onBlur"
            (const $ pure TodoCancelEdit)
            hdlBlur

    hdlBlur ::
        (F.MonadReactor m, HasItem' TodoInfo s)
        => F.SceneHandler m v s TodoCancelEdit (Which '[])
    hdlBlur this@(F.Obj ref its) _ = F.terminate' . lift $ do
        F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"completed" %~ not)
        F.rerender' this

    onKeyDown ::
        ( F.MonadReactor m
        , F.MonadJS m
        , HasItemTag' TodoInput [F.Listener] s
        , HasItem' TodoInfo s
        ) => F.SceneActivator m v s (Which '[TodoDestroy])
    onKeyDown = F.controlledTrigger @TodoInput
            "onKeyDown"
            (runMaybeT . F.fireKeyDownKey)
            (F.maybeHandle hdlKeyDown)

    hdlKeyDown ::
        ( F.MonadReactor m
        , F.MonadJS m
        , HasItem' TodoInfo s
        )
        => F.SceneHandler m v s F.KeyDownKey (Which '[TodoDestroy])
    hdlKeyDown this@(F.Obj ref its) (F.KeyDownKey j key) = ContT $ \fire ->
        case key of
            "Enter" -> do
                v <- JE.fromJS' @J.JSString <$> (F.doGetProperty "value" (JE.toJS j))
                let v' = J.strip $ fromMaybe J.empty v
                if J.null v'
                    then
                        fire $ pickOnly TodoDestroy
                    else do
                        F.doModifyIORef' ref (\s -> s
                            & its.F.model.item' @TodoInfo .field @"editing" .~ False
                            & its.F.model.item' @TodoInfo .field @"value" .~ v')
                        F.rerender this (F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j))
            "Escape" -> do
                F.doModifyIORef' ref (its.F.model.item' @TodoInfo .field @"editing" .~ False)
                -- don't strictly need to reset the value, editing = false will hide this input
                -- but it's nice to reset the the dom
                F.rerender this (F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j))
            _ -> pure ()

    hdlStartEdit ::
        ( F.MonadReactor m
        , F.MonadJS m
        , F.MonadHTMLElement m
        , HasItem' TodoInfo s
        , HasItemTag' TodoInput F.EventTarget s)
        => F.SceneHandler m v s TodoStartEdit (Which '[])
    hdlStartEdit this@(F.Obj ref its) _ = F.terminate' $ lift $ do
        void $ runMaybeT $ do
            obj <- lift $ F.doReadIORef ref
            let b = obj ^. its.F.model.item' @TodoInfo .field @"completed"
            guard (not b)
            -- Focus after rendering changed because we are using uncontrollec components
            -- with a new key. This will result in a different input element after each render
            lift $ F.doModifyIORef' ref (\s -> s
                & its.F.model.item' @TodoInfo .field @"editing" .~ True
                )
            lift . F.rerender this $ do
                obj' <- F.doReadIORef ref
                let j = obj' ^. its.F.model.itemTag' @TodoInput @F.EventTarget
                F.doSetProperty ("value", JE.toJS' J.empty) (JE.toJS j)
                F.focusRef @TodoInput this

todo ::
        (F.MonadReactor m
        , F.MonadJS m
        , F.MonadHTMLElement m
        , HasItemTag' TodoDestroy [F.Listener] s
        , HasItemTag' TodoInput F.EventTarget s
        , HasItemTag' TodoInput [F.Listener] s
        , HasItemTag' TodoLabel [F.Listener] s
        , HasItemTag' TodoToggleComplete [F.Listener] s
        , HasItem' TodoInfo s) =>
        F.Prototype m v i s
        (Many '[])
        (Many
            '[ Tagged TodoToggleComplete [F.Listener]
             , Tagged TodoDestroy [F.Listener]
             , Tagged TodoLabel [F.Listener]
             , Tagged TodoInput [F.Listener]
             , Tagged TodoInput F.EventTarget
             , Tagged TodoLabel [F.Listener]])
        (Which '[TodoDestroy])
        (Which '[])
        (Which '[])
todo = let  p = todoView
                `F.andPrototype` todoInput
                `F.andPrototype` todoLabel
            act = F.activator' p
            hdl = F.handler' p
            disp = F.display' p
        in p { F.activator' = act `F.drives'` hdl
            , F.handler' = F.nulHandler
            , F.display' = \s ->
                let s' = s ^. F.model.item' @TodoInfo
                in F.branch "div" []
                    [ ("className", JE.classNames
                        [ ("completed", completed s')
                        , ("editing", editing s')])
                    ]
                    (disp s)
            }


-- data GetProperty x m = GetProperty J.JSString J.JSVal (JE.JSVar -> m ())

-- wock ::
--     ( HasItemTag' TodoCheckbox [F.Listener] s
--     , HasItem' TodoInfo s
--     , F.MonadReactor m
--     ) => F.ProtoActivator x m v s (Which '[C.Rerender])
-- wock = F.withExecutor pickOnly wack

-- wick :: ( HasItemTag' TodoCheckbox [F.Listener] s
--     , HasItem' TodoInfo s
--     , F.MonadReactor m
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
--     , F.MonadReactor m
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
--     , F.MonadReactor m
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
