{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Todo where

import qualified Control.Disposable as CD
import Control.Lens
import Data.Generics.Product
import Data.Diverse.Profunctor
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier.React as R
import qualified Glazier.React.Framework as F
import qualified JavaScript.Extras as JE

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

instance CD.Dispose TodoInfo

data TodoPlan = TodoPlan
    { fireToggleComplete :: Maybe (J.Callback (J.JSVal -> IO ()))
    , fireStartEdit :: Maybe (J.Callback (J.JSVal -> IO ()))
    , fireDestroy :: Maybe (J.Callback (J.JSVal -> IO ()))
    , fireCancelEdit :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onEditKeyDown :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onComponentRef :: Maybe (J.Callback (J.JSVal -> IO ()))
    , onEditRef :: Maybe (J.Callback (J.JSVal -> IO ()))
    } deriving G.Generic

instance CD.Dispose TodoPlan


type TodoModel = (TodoPlan, TodoInfo)


-- type Model = Schema
-- type Outline = Schema
-- instance R.ToOutline Model Outline where outline = id

-- mkModel :: Outline -> F (R.Maker Action) Model
-- mkModel = pure

-- data Plan = Plan
--     { _component :: R.ReactComponent
--     , _key :: J.JSString
--     , _frameNum :: Int
--     , _componentRef :: J.JSVal
--     , _editRef :: J.JSVal
--     , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
--     , _onComponentRef :: J.Callback (J.JSVal -> IO ())
--     , _onEditRef :: J.Callback (J.JSVal -> IO ())
--     , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
--     , _onEditRef :: J.Callback (J.JSVal -> IO ())
--     , _fireToggleComplete :: J.Callback (J.JSVal -> IO ())
--     , _fireStartEdit :: J.Callback (J.JSVal -> IO ())
--     , _fireDestroy :: J.Callback (J.JSVal -> IO ())
--     , _fireCancelEdit :: J.Callback (J.JSVal -> IO ())
--     , _onEditKeyDown :: J.Callback (J.JSVal -> IO ())
--     } deriving G.Generic

-- makeClassyPrisms ''Action
-- makeClassy ''Plan
-- makeClassy ''Schema

-- mkPlan :: R.Frame Model Plan -> F (R.Maker Action) Plan
-- mkPlan mm = Plan
--     <$> R.getComponent
--     <*> R.mkKey
--     <*> pure 0
--     <*> pure J.nullRef
--     <*> pure J.nullRef
--     <*> (R.mkRenderer mm $ const render)
--     <*> (R.mkHandler $ pure . pure . ComponentRefAction)
--     <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)
--     <*> (R.mkHandler $ pure . pure . EditRefAction)
--     <*> (R.mkHandler $ pure . pure . const ToggleCompletedAction)
--     <*> (R.mkHandler $ pure . pure . const StartEditAction)
--     <*> (R.mkHandler $ pure . pure . const DestroyAction)
--     -- <*> (R.mkHandler $ pure . pure . const CancelEditAction)
--     <*> (R.mkHandler onCancelEdit')
--     <*> (R.mkHandler onEditKeyDown')

-- instance CD.Disposing Plan
-- instance CD.Disposing Model where
--     disposing _ = CD.DisposeNone

-- -- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
-- instance HasPlan (R.Scene Model Plan) where
--     plan = R.plan
-- instance HasSchema (R.Scene Model Plan) where
--     schema = R.model
-- instance HasPlan (R.Gizmo Model Plan) where
--     plan = R.scene . plan
-- instance HasSchema (R.Gizmo Model Plan) where
--     schema = R.scene . schema

-- type Widget = R.Widget Action Outline Model Plan Command
-- widget :: Widget
-- widget = R.Widget
--     mkModel
--     mkPlan
--     window
--     gadget

todoDisplay :: ( R.MonadReactor x m, HasItem' TodoModel ss) => F.Display m (F.ComponentPlan, ss) ()
todoDisplay = F.Display $ \(cp, ss) -> do
    let (p, i) = ss ^. item' @TodoModel
    R.bh "div" []
        [ ("className", JE.classNames [ ("completed", i ^. field @"completed")
                                      , ("editing", i ^. field @"editing")])
        ] $ do
       R.bh "div" [] [ ("key", "view")
                     , ("className", "view")
                     ] $ do
            R.lf "input"
                (JE.justSnds $ [ ("onChange", p ^. field @"fireToggleComplete")
                               ])
                [ ("key", "toggle")
                , ("className", "toggle")
                , ("type", "checkbox")
                , ("checked", i ^. field @"completed" . to JE.toJS')
                ]
            R.bh "label"
                (JE.justSnds $ [ ("onDoubleClick", p ^. field @"fireStartEdit")
                               ])
                [ ("key", "label")
                ] (i ^. field @"value" . to R.txt)
            R.lf "button"
                (JE.justSnds $ [ ("onClick", p ^. field @"fireDestroy")
                               ])
                [ ("key", "destroy")
                , ("className", "destroy")
                ]
       -- For uncontrolled components, we need to generate a new key per render
       -- in order for react to use the new defaultValue
       R.lf "input"
           (JE.justSnds $
               [ ("ref", p ^. field @"onEditRef")
               , ("onBlur", p ^. field @"fireCancelEdit")
               , ("onKeyDown", p ^. field @"onEditKeyDown")
               ])
           [ ("key", JE.toJS' $ J.unwords
                                       [ cp ^. field @"key" . to R.runReactKey
                                       , cp ^. field @"frameNum" . to show . to J.pack
                                       ])
           , ("className", "edit")
           , ("defaultValue", i ^. field @"value" . to JE.toJS')
           , ("defaultChecked", i ^. field @"completed" . to JE.toJS')
           ]


-- | This is used by parent components to render this component
-- window :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
-- window = do
--     s <- ask
--     lift $ R.lf (s ^. component . to JE.toJS')
--         [ ("key",  s ^. key . to JE.toJS')
--         , ("render", s ^. onRender . to JE.toJS')
--         , ("ref", s ^. onComponentRef . to JE.toJS')
--         , ("componentDidUpdate", s ^. onComponentDidUpdate . to JE.toJS')
--         ]

-- render :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
-- render = do
--     s <- ask
--     lift $ R.bh "li" [ ("className", classNames [ ("completed", s ^. completed)
--                                                 , ("editing", s ^. editing)])
--                      ] $ do
--         R.bh "div" [ ("key", "view")
--                    , ("className", "view")
--                    ] $ do
--             R.lf "input" [ ("key", "toggle")
--                          , ("className", "toggle")
--                          , ("type", "checkbox")
--                          , ("checked", s ^. completed . to JE.toJS')
--                          , ("onChange", s ^. fireToggleComplete . to JE.toJS')
--                          ]
--             R.bh "label"  [ ("key", "label")
--                           , ("onDoubleClick", s ^. fireStartEdit. to JE.toJS')
--                           ] (s ^. value . to R.txt)
--             R.lf "button" [ ("key", "destroy")
--                           , ("className", "destroy")
--                           , ("onClick", s ^. fireDestroy . to JE.toJS')
--                           ]
--         -- For uncontrolled components, we need to generate a new key per render
--         -- in order for react to use the new defaultValue
--         R.lf "input" [ ("key", JE.toJS' $ J.unwords
--                                        [ s ^. key
--                                        , s ^. frameNum . to show . to J.pack
--                                        ])
--                      , ("ref", s ^.  onEditRef . to JE.toJS')
--                      , ("className", "edit")
--                      , ("defaultValue", s ^. value . to JE.toJS')
--                      , ("defaultChecked", s ^. completed . to JE.toJS')
--                      , ("onBlur", s ^. fireCancelEdit . to JE.toJS')
--                      , ("onKeyDown", s ^. onEditKeyDown . to JE.toJS')
--                      ]


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
