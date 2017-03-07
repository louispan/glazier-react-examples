{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Input
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Gasket(..)
    , HasGasket(..)
    , mkGasket
    , Model(..)
    , HasModel(..)
    , GModel
    , MModel
    , SuperModel
    , mkSuperModel
    , window
    , gadget
    ) where

import Control.Concurrent.MVar
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model.Class as R
import qualified JavaScript.Extras as JE
import qualified Todo.Gadget as TD

data Command
    -- Common widget commands
    = SetPropertyCommand JE.Property J.JSVal
    -- widget specific commands
    | SubmitCommand J.JSString

data Action
    -- Common widget actions
    = SendCommandsAction [Command]
    -- widget specific actions
    | SubmitAction J.JSString

data Model = Model
    -- common widget model
    { _uid :: J.JSString
    -- widget specifc model
    , _placeholder :: J.JSString
    }

data Gasket = Gasket
    -- common widget callbacks
    { _component :: R.ReactComponent
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    -- widget specific callbacks
    , _onKeyDown :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

----------------------------------------------------------
-- The following should be the same per widget
-- | Gasket and pure state
type GModel = (Gasket, Model)
-- | Mutable model for rendering callback
type MModel = MVar GModel
-- | Contains MModel and GModel
type SuperModel = (MModel, GModel)
makeClassyPrisms ''Action
makeClassy ''Gasket
makeClassy ''Model
instance CD.Disposing Gasket
-- GModel
instance R.HasGModel GModel GModel where
    gModel = id
instance HasGasket GModel where
    gasket = _1
instance HasModel GModel where
    model = _2
instance CD.Disposing GModel
-- MModel
instance R.HasMModel MModel GModel where
    mModel = id
-- SuperModel
instance R.HasMModel SuperModel GModel where
    mModel = _1
instance R.HasGModel SuperModel GModel where
    gModel = _2
instance HasGasket SuperModel where
    gasket = R.gModel . gasket
instance HasModel SuperModel where
    model = R.gModel . model
instance CD.Disposing SuperModel where
    disposing s = CD.disposing $ s ^. R.gModel
-- End same code per widget
----------------------------------------------------------


-- | This might be different per widget
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

mkSuperModel :: Model -> F (R.Maker Action) SuperModel
mkSuperModel s = R.mkSuperModel mkGasket $ \cbs -> (cbs, s)
-- End similar code per widget
----------------------------------------------------------

mkGasket :: MVar GModel -> F (R.Maker Action) Gasket
mkGasket ms = Gasket
    -- common widget callbacks
    <$> R.getComponent
    <*> (R.mkRenderer ms $ const render)
    -- widget specific callbacks
    <*> (R.mkHandler onKeyDown')

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to J.pToJSVal)
        [ ("key",  s ^. uid . to J.pToJSVal)
        , ("render", s ^. onRender . to JE.PureJSVal . to J.pToJSVal)
        ]

-- | This is used by the React render callback
render :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.lf (JE.strval "input")
                    [ ("key", s ^. uid . to J.jsval)
                    , ("className", JE.strval "new-todo")
                    , ("placeholder", s ^. placeholder . to J.jsval)
                    , ("autoFocus", J.pToJSVal True)
                    , ("onKeyDown", s ^. onKeyDown . to J.jsval)
                    ]

onKeyDown' :: J.JSVal -> MaybeT IO [Action]
onKeyDown' = R.eventHandlerM TD.onInputKeyDown goLazy
  where
    goLazy :: (Maybe J.JSString, J.JSVal) -> MaybeT IO [Action]
    goLazy (ms, j) = pure $
        SendCommandsAction [SetPropertyCommand ("value", J.pToJSVal J.empty) j]
        : maybe [] (pure . SubmitAction) ms

-- | State update logic.
-- The best practice is to leave this in general Monad m (eg, not MonadIO).
-- This allows gadget to use STM as the base monad which allows for combining concurrently
-- with other stateful STM effects and still maintain a single source of truth.
gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
gadget = do
    a <- ask
    case a of
        -- common widget actions
        SendCommandsAction cmds -> pure $ D.fromList cmds

        -- widget specific actions
        SubmitAction v -> do
            let v' = J.strip v
            if J.null v'
                then pure mempty
                else pure $ D.singleton $ SubmitCommand v'
