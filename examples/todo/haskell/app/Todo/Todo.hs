{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Todo.Todo
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Plan(..)
    , HasPlan(..)
    , mkPlan
    , Model(..)
    , HasModel(..)
    , Design
    , Frame
    , SuperModel
    , Widget
    , widget
    , window
    , gadget
    ) where

import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Nullable as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Command as R
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified Glazier.React.Widget as R
import qualified Glazier.React.Widgets.Input as W.Input
import qualified JavaScript.Extras as JE

data Command
    = RenderCommand (R.SuperModel Model Plan) [JE.Property] J.JSVal
    | SetPropertyCommand JE.Property J.JSVal
    | FocusNodeCommand J.JSVal
    | SendActionsCommand [Action]

data Action
    = ComponentRefAction J.JSVal
    | RenderAction
    | ComponentDidUpdateAction
    | SendCommandsAction [Command]
    | EditRefAction J.JSVal
    | StartEditAction
    | FocusEditAction
    | ToggleCompletedAction
    | SetCompletedAction Bool
    | DestroyAction
    | CancelEditAction
    | SubmitAction J.JSString

data Model = Model
    { _key :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _deferredActions :: D.DList Action
    , _editRef :: J.JSVal
    , _value :: J.JSString
    , _completed :: Bool
    , _editing :: Bool
    }

instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

data Plan = Plan
    { _component :: R.ReactComponent
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    , _onEditRef :: J.Callback (J.JSVal -> IO ())
    , _fireToggleComplete :: J.Callback (J.JSVal -> IO ())
    , _fireStartEdit :: J.Callback (J.JSVal -> IO ())
    , _fireDestroy :: J.Callback (J.JSVal -> IO ())
    , _fireCancelEdit :: J.Callback (J.JSVal -> IO ())
    , _onEditKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving G.Generic

instance CD.Disposing Plan
makeClassyPrisms ''Action
makeClassy ''Plan
makeClassy ''Model

mkPlan :: R.Frame Model Plan -> F (R.Maker Action) Plan
mkPlan mm = Plan
    <$> R.getComponent
    <*> (R.mkRenderer mm $ const render)
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)
    <*> (R.mkHandler $ pure . pure . EditRefAction)
    <*> (R.mkHandler $ pure . pure . const ToggleCompletedAction)
    <*> (R.mkHandler $ pure . pure . const StartEditAction)
    <*> (R.mkHandler $ pure . pure . const DestroyAction)
    <*> (R.mkHandler $ pure . pure . const CancelEditAction)
    <*> (R.mkHandler onEditKeyDown')

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Design Model Plan) where
    plan = R.plan
instance HasModel (R.Design Model Plan) where
    model = R.model
instance HasPlan (R.SuperModel Model Plan) where
    plan = R.design . plan
instance HasModel (R.SuperModel Model Plan) where
    model = R.design . model

type Design = R.Design Model Plan
type Frame = R.Frame Model Plan
type SuperModel = R.SuperModel Model Plan

type Widget = R.Widget Command Action Model Plan
widget :: R.Widget Command Action Model Plan
widget = R.Widget
    mkPlan
    window
    gadget

-- | This is used by parent components to render this component
window :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS)
        [ ("key",  s ^. key . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        , ("ref", s ^. onComponentRef . to JE.toJS)
        , ("componentDidUpdate", s ^. onComponentDidUpdate . to JE.toJS)
        ]

render :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
render = do
    s <- ask
    lift $ R.bh (JE.strJS "li") [ ("className", classNames [ ("completed", s ^. completed)
                                                            , ("editing", s ^. editing)])
                                 ] $ do
        R.bh (JE.strJS "div") [ ("key", JE.strJS "view")
                               , ("className", JE.strJS "view")
                               ] $ do
            R.lf (JE.strJS "input") [ ("key", JE.strJS "toggle")
                                    , ("className", JE.strJS "toggle")
                                    , ("type", JE.strJS "checkbox")
                                    , ("checked", s ^. completed . to JE.toJS)
                                    , ("onChange", s ^. fireToggleComplete . to JE.toJS)
                                    ]
            R.bh (JE.strJS "label")  [ ("key", JE.strJS "label")
                                      , ("onDoubleClick", s ^. fireStartEdit. to JE.toJS)
                                      ] (s ^. value . to R.txt)
            R.lf (JE.strJS "button") [ ("key", JE.strJS "destroy")
                                      , ("className", JE.strJS "destroy")
                                      , ("onClick", s ^. fireDestroy . to JE.toJS)
                                      ]
        -- For uncontrolled components, we need to generate a new key per render
        -- in order for react to use the new defaultValue
        R.lf (JE.strJS "input") [ ("key", JE.toJS $ J.unwords
                                       [ s ^. key
                                       , s ^. frameNum . to show . to J.pack
                                       ])
                                , ("ref", s ^.  onEditRef . to JE.toJS)
                                , ("className", JE.strJS "edit")
                                , ("defaultValue", s ^. value . to JE.toJS)
                                , ("defaultChecked", s ^. completed . to JE.toJS)
                                , ("onBlur", s ^. fireCancelEdit . to JE.toJS)
                                , ("onKeyDown", s ^. onEditKeyDown . to JE.toJS)
                                ]

classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = JE.toJS . J.unwords . fmap fst . filter snd

onEditKeyDown' :: J.JSVal -> MaybeT IO [Action]
onEditKeyDown' = R.eventHandlerM W.Input.whenKeyDown goLazy
  where
    goLazy :: (Maybe J.JSString, J.JSVal) -> MaybeT IO [Action]
    goLazy (ms, j) = pure $
        SendCommandsAction [SetPropertyCommand ("value", JE.toJS J.empty) j]
        : maybe [CancelEditAction] (pure . SubmitAction) ms

gadget :: G.GadgetT Action (R.SuperModel Model Plan) Identity (D.DList Command)
gadget = do
    a <- ask
    case a of
        -- common widget actions

        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        RenderAction ->
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

        ComponentDidUpdateAction -> do
            -- Run delayed action that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            acts <- use deferredActions
            deferredActions .= mempty
            -- st :: Action -> StateT SuperModel m (D.DList Command)
            pure $ D.singleton $ SendActionsCommand $ D.toList acts

        SendCommandsAction cmds -> pure $ D.fromList cmds

        -- widget specific actions
        -- Focus after rendering changed because a new input element might have been rendered
        FocusEditAction -> do
            input <- use editRef
            ret <- runMaybeT $ do
                input' <- MaybeT $ pure $ J.nullableToMaybe (J.Nullable input)
                pure $ D.singleton $ FocusNodeCommand input'
            maybe (pure mempty) pure ret

        EditRefAction v -> do
            editRef .= v
            pure mempty

        ToggleCompletedAction -> do
            completed %= not
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

        SetCompletedAction b -> do
            completed .= b
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

        StartEditAction -> do
            ret <- runMaybeT $ do
                b <- use completed
                guard (not b)
                editing .= True
                -- Need to delay focusing until after the next render
                deferredActions %= (`D.snoc` FocusEditAction)
                D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
            maybe (pure mempty) pure ret

        -- parent widgets should detect this case to do something with submitted action
        DestroyAction -> pure mempty

        CancelEditAction -> do
            editing .= False
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

        SubmitAction v -> do
            -- trim the text
            let v' = J.strip v
            value .= v'
            editing .= False
            if J.null v'
                then pure $ D.singleton $ SendActionsCommand [DestroyAction]
                else D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
