{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Todo.Footer
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
    , onHashChange
    , whenHashChange
    , withHashChange
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
import qualified GHCJS.Marshal as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Command as R
import qualified Glazier.React.Component as R
import qualified Glazier.React.Event as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE
import Prelude hiding (filter)
import qualified Prelude as P
import qualified Todo.Filter as TD.Filter

data Command
    = RenderCommand (R.SuperModel Model Plan) [JE.Property] J.JSVal

data Action
    = ComponentRefAction J.JSVal
    | RenderAction
    | ClearCompletedAction
    | SetFilterAction TD.Filter.Filter
    | SetCountsAction Int Int

data Model = Model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _activeCount :: Int
    , _completedCount :: Int
    , _filter :: TD.Filter.Filter
    }

instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

data Plan = Plan
    { _component :: R.ReactComponent
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _fireClearCompleted :: J.Callback (J.JSVal -> IO ())
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
    <*> (R.mkHandler $ pure . pure . const ClearCompletedAction)

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
        [ ("key",  s ^. uid . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        , ("ref", s ^. onComponentRef . to JE.toJS)
        ]

render :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
render = do
    s <- ask
    lift $ R.bh (JE.strJS "footer") [("className", JE.strJS "footer")] $ do
        R.bh (JE.strJS "span") [ ("className", JE.strJS "todo-count")
                                , ("key", JE.strJS "todo-count")] $ do
            R.bh (JE.strJS "strong") [("key", JE.strJS "items")] (s ^. activeCount . to show . to J.pack . to R.txt)
            R.txt " items left"
        R.bh (JE.strJS "ul") [("className", JE.strJS "filters")
                              , ("key", JE.strJS "filters")] $ do
            R.bh (JE.strJS "li") [("key", JE.strJS "filter-all")] $
                R.bh (JE.strJS "a")
                [ ("href", JE.strJS "#/")
                , ("key", JE.strJS "all")
                , ("className", classNames [("selected", s ^. filter == TD.Filter.All)])
                ] $
                R.txt "All"
            (R.txt " ")
            R.bh (JE.strJS "li") [("key", JE.strJS "filter-active")] $
                R.bh (JE.strJS "a")
                [ ("href", JE.strJS "#/active")
                , ("key", JE.strJS "active")
                , ("className", classNames [("selected", s ^. filter == TD.Filter.Active)])
                ] $
                R.txt "Active"
            (R.txt " ")
            R.bh (JE.strJS "li") [("key", JE.strJS "filter-completed")] $
                R.bh (JE.strJS "a")
                [ ("href", JE.strJS "#/completed")
                , ("key", JE.strJS "completed")
                , ("className", classNames [("selected", s ^. filter == TD.Filter.Completed)])
                ] $
                R.txt "Completed"
        if (s ^. completedCount > 0)
           then R.bh (JE.strJS "button")  [("key", JE.strJS "clear-completed")
                                          , ("className", JE.strJS "clear-completed")
                                          , ("onClick", s ^. fireClearCompleted . to JE.toJS)] $
                (R.txt "Clear completed")
           else mempty

classNames :: [(J.JSString, Bool)] -> J.JSVal
classNames = JE.toJS . J.unwords . fmap fst . P.filter snd

gadget :: G.GadgetT Action (R.SuperModel Model Plan) Identity (D.DList Command)
gadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        RenderAction ->
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

        -- parent widget need to handle this action explicitly
        ClearCompletedAction -> pure mempty

        SetFilterAction ftr -> do
            filter .= ftr
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

        SetCountsAction activeCnt completedCnt -> do
            activeCount .= activeCnt
            completedCount .= completedCnt
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

-- | This needs to be explictly registered by the Main app
onHashChange ::  J.JSVal -> MaybeT IO [Action]
onHashChange = R.eventHandlerM whenHashChange withHashChange

-- | Provide split up parts of onHashChange in case the applications
-- needs to combine other widgets that also uses hashchange event
whenHashChange :: J.JSVal -> MaybeT IO J.JSString
whenHashChange evt = do
    newURL <- MaybeT (JE.getProperty "newURL" evt >>= J.fromJSVal) -- FIXME: Use Nullable?
    let (_, newHash) = J.breakOn "#" newURL
    pure newHash

-- | Provide split up parts of onHashChange in case the applications
-- needs to combine other widgets that also uses hashchange event
withHashChange :: J.JSString -> MaybeT IO [Action]
withHashChange newHash =
    case newHash of
        "#/active" -> pure [SetFilterAction TD.Filter.Active]
        "#/completed" -> pure [SetFilterAction TD.Filter.Completed]
        _ -> pure [SetFilterAction TD.Filter.All]
