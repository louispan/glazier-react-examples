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
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Nullable as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE
import qualified Todo.Gadget as TD

data Command
    -- Common widget commands
    = RenderCommand (R.SuperModel Gasket Model) [JE.Property] J.JSVal
    -- widget specific commands
    | SetPropertyCommand JE.Property J.JSVal
    | FocusNodeCommand J.JSVal
    | DestroyCommand

data Action
    -- Common widget actions
    = ComponentRefAction J.JSVal
    | ComponentDidUpdateAction
    | SendCommandsAction [Command]
    -- widget specific actions
    | EditRefAction J.JSVal
    | StartEditAction
    | FocusEditAction
    | ToggleCompletedAction
    | SetCompletedAction Bool
    | DestroyAction
    | CancelEditAction
    | SubmitAction J.JSString

data Model = Model
    -- common widget model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _deferredActions :: D.DList Action
    -- widget specifc model
    , _editRef :: J.JSVal
    , _value :: J.JSString
    , _completed :: Bool
    , _editing :: Bool
    }

data Gasket = Gasket
    -- common widget callbacks
    { _component :: R.ReactComponent
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    -- widget specific callbacks
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
    -- common widget callbacks
    <$> R.getComponent
    <*> (R.mkRenderer mm $ const render)
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)
    -- widget specific callbacks
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
    lift $ R.lf (s ^. component . to J.pToJSVal)
        [ ("key",  s ^. uid . to J.pToJSVal)
        , ("render", s ^. onRender . to JE.PureJSVal . to J.pToJSVal)
        , ("ref", s ^. onComponentRef . to JE.PureJSVal . to J.pToJSVal)
        , ("componentDidUpdate", s ^. onComponentDidUpdate . to JE.PureJSVal . to J.pToJSVal)
        ]

render :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.bh (JE.strval "li") [ ("className"
                                 , classNames [("completed", s ^. completed), ("editing", s ^. editing)])] $ do
        R.bh (JE.strval "div") [ ("key", JE.strval "view")
                              , ("className", JE.strval "view")
                              ] $ do
            R.lf (JE.strval "input") [ ("key", JE.strval "toggle")
                                    , ("className", JE.strval "toggle")
                                    , ("type", JE.strval "checkbox")
                                    , ("checked", s ^. completed . to J.pToJSVal)
                                    , ("onChange", s ^. fireToggleComplete . to J.jsval)
                                    ]
            R.bh (JE.strval "label")  [ ("key", JE.strval "label")
                                     , ("onDoubleClick", s ^. fireStartEdit. to J.jsval)
                                     ] (s ^. value . to R.txt)
            R.lf (JE.strval "button") [ ("key", JE.strval "destroy")
                                     , ("className", JE.strval "destroy")
                                     , ("onClick", s ^. fireDestroy . to J.jsval)
                                     ]
        -- For uncontrolled components, we need to generate a new key per render
        -- in for react to use the new defaultValue
        R.lf (JE.strval "input") [ ("key", J.jsval $ J.unwords
                                       [ s ^. uid
                                       , s ^. completed . to show . to J.pack
                                       , s ^.  frameNum . to show . to J.pack
                                       ])
                                , ("ref", s ^.  onEditRef . to J.jsval)
                                , ("className", JE.strval "edit")
                                , ("defaultValue", s ^. value . to J.jsval)
                                , ("defaultChecked", s ^. completed . to J.pToJSVal)
                                , ("onBlur", s ^. fireCancelEdit . to J.jsval)
                                , ("onKeyDown", s ^. onEditKeyDown . to J.jsval)
                                ]

classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = J.jsval . J.unwords . fmap fst . filter snd

onEditKeyDown' :: J.JSVal -> MaybeT IO [Action]
onEditKeyDown' = R.eventHandlerM TD.onInputKeyDown goLazy
  where
    goLazy :: (Maybe J.JSString, J.JSVal) -> MaybeT IO [Action]
    goLazy (ms, j) = pure $
        SendCommandsAction [SetPropertyCommand ("value", J.pToJSVal J.empty) j]
        : maybe [CancelEditAction] (pure . SubmitAction) ms

gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
gadget = do
    a <- ask
    case a of
        -- common widget actions

        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        ComponentDidUpdateAction -> do
            -- Run delayed action that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            acts <- use deferredActions
            deferredActions .= mempty
            -- st :: Action -> StateT SuperModel m (D.DList Command)
            let st = runReaderT (G.runGadgetT gadget)
            G.GadgetT (lift (fold <$> (traverse st (D.toList acts))))

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
            D.singleton <$> renderCmd

        SetCompletedAction b -> do
            completed .= b
            D.singleton <$> renderCmd

        StartEditAction -> do
            ret <- runMaybeT $ do
                b <- use completed
                guard (not b)
                editing .= True
                -- Need to delay focusing until after the next render
                deferredActions %= (`D.snoc` FocusEditAction)
                lift $ D.singleton <$> renderCmd
            maybe (pure mempty) pure ret

        DestroyAction -> pure $ D.singleton DestroyCommand

        CancelEditAction -> do
            editing .= False
            D.singleton <$> renderCmd

        SubmitAction v -> do
            -- trim the text
            let v' = J.strip v
            value .= v'
            editing .= False
            if J.null v'
                then pure $ D.singleton DestroyCommand
                else D.singleton <$> renderCmd

-- | Just change the state to something different so the React pureComponent will call render()
renderCmd :: Monad m => G.GadgetT Action SuperModel m Command
renderCmd = do
    frameNum %= (\i -> (i + 1) `mod` 100)
    i <- J.pToJSVal <$> use frameNum
    r <- use (model . componentRef)
    sm <- get
    pure $ RenderCommand sm [("frameNum", i)] r
