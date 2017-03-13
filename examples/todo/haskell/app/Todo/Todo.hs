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
    , Gasket(..)
    , HasGasket(..)
    , mkGasket
    , Model(..)
    , HasModel(..)
    , mkSuperModel
    , Widget
    , GModel
    , MModel
    , SuperModel
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
import qualified Glazier.React.Widget as R
import qualified Glazier.React.Widgets.Input as W.Input
import qualified JavaScript.Extras as JE

data Command
    = RenderCommand (R.SuperModel Gasket Model) [JE.Property] J.JSVal
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
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _deferredActions :: D.DList Action
    , _editRef :: J.JSVal
    , _value :: J.JSString
    , _completed :: Bool
    , _editing :: Bool
    }

data Gasket = Gasket
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

makeClassyPrisms ''Action
makeClassy ''Gasket
makeClassy ''Model

mkGasket :: R.MModel Gasket Model -> F (R.Maker Action) Gasket
mkGasket mm = Gasket
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

instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

mkSuperModel :: Model -> F (R.Maker Action) SuperModel
mkSuperModel s = R.mkSuperModel mkGasket $ \gsk -> R.GModel gsk s

data Widget
instance R.IsWidget Widget where
    type WidgetAction Widget = Action
    type WidgetCommand Widget = Command
    type WidgetModel Widget = Model
    type WidgetGasket Widget = Gasket
type GModel = R.WidgetGModel Widget
type MModel = R.WidgetMModel Widget
type SuperModel = R.WidgetSuperModel Widget

----------------------------------------------------------
-- The following should be the same per widget (except for type params)
instance CD.Disposing Gasket
instance HasGasket (R.GModel Gasket Model) where
    gasket = R.widgetGasket
instance HasModel (R.GModel Gasket Model) where
    model = R.widgetModel
instance HasGasket (R.SuperModel Gasket Model) where
    gasket = R.gModel . gasket
instance HasModel (R.SuperModel Gasket Model) where
    model = R.gModel . model
-- End same code per widget
----------------------------------------------------------

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS)
        [ ("key",  s ^. uid . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        , ("ref", s ^. onComponentRef . to JE.toJS)
        , ("componentDidUpdate", s ^. onComponentDidUpdate . to JE.toJS)
        ]

render :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
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
                                       [ s ^. uid
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

gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
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
