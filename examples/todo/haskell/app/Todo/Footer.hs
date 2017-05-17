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
    , Schema(..)
    , HasSchema(..)
    , Plan(..)
    , HasPlan(..)
    , Outline
    , Model
    , Widget
    , widget
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
    = RenderCommand (R.Gizmo Model Plan) [JE.Property] J.JSVal

data Action
    = ComponentRefAction J.JSVal
    | RenderAction
    | ClearCompletedAction
    | SetFilterAction TD.Filter.Filter
    | SetCountsAction Int Int

data Schema = Schema
    { _activeCount :: Int
    , _completedCount :: Int
    , _filter :: TD.Filter.Filter
    }

type Model = Schema
type Outline = Schema
instance R.ToOutline Model Outline where outline = id

mkModel :: Outline -> F (R.Maker Action) Model
mkModel = pure

data Plan = Plan
    { _component :: R.ReactComponent
    , _key :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _fireClearCompleted :: J.Callback (J.JSVal -> IO ())
    } deriving G.Generic

makeClassyPrisms ''Action
makeClassy ''Plan
makeClassy ''Schema

mkPlan :: R.Frame Model Plan -> F (R.Maker Action) Plan
mkPlan mm = Plan
    <$> R.getComponent
    <*> R.mkKey
    <*> pure J.nullRef
    <*> pure 0
    <*> (R.mkRenderer mm $ const render)
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ClearCompletedAction)

instance CD.Disposing Plan
instance CD.Disposing Model where
    disposing _ = CD.DisposeNone

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Scene Model Plan) where
    plan = R.plan
instance HasSchema (R.Scene Model Plan) where
    schema = R.model
instance HasPlan (R.Gizmo Model Plan) where
    plan = R.scene . plan
instance HasSchema (R.Gizmo Model Plan) where
    schema = R.scene . schema

type Widget = R.Widget () Command Action Outline Model Plan
widget :: Widget
widget = R.Widget
    mkModel
    mkPlan
    window
    gadget

-- | This is used by parent components to render this component
window :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS')
        [ ("key",  s ^. key . to JE.toJS')
        , ("render", s ^. onRender . to JE.toJS')
        , ("ref", s ^. onComponentRef . to JE.toJS')
        ]

render :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
render = do
    s <- ask
    lift $ R.bh "footer" [("className", "footer")] $ do
        R.bh "span" [ ("className", "todo-count")
                    , ("key", "todo-count")] $ do
            R.bh "strong" [("key", "items")] (s ^. activeCount . to show . to J.pack . to R.txt)
            R.txt " items left"
        R.bh "ul" [("className", "filters")
                  , ("key", "filters")] $ do
            R.bh "li" [("key", "filter-all")] $
                R.bh "a" [ ("href", "#/")
                         , ("key", "all")
                         , ("className", classNames [("selected", s ^. filter == TD.Filter.All)])
                         ] $
                R.txt "All"
            R.txt " "
            R.bh "li" [("key", "filter-active")] $
                R.bh "a"
                [ ("href", "#/active")
                , ("key", "active")
                , ("className", classNames [("selected", s ^. filter == TD.Filter.Active)])
                ] $
                R.txt "Active"
            R.txt " "
            R.bh "li" [("key", "filter-completed")] $
                R.bh "a"
                [ ("href", "#/completed")
                , ("key", "completed")
                , ("className", classNames [("selected", s ^. filter == TD.Filter.Completed)])
                ] $
                R.txt "Completed"
        if (s ^. completedCount > 0)
           then R.bh "button" [("key", "clear-completed")
                              , ("className", "clear-completed")
                              , ("onClick", s ^. fireClearCompleted . to JE.toJS')] $
                    R.txt "Clear completed"
           else mempty

classNames :: [(J.JSString, Bool)] -> JE.JSVar
classNames = JE.toJS' . J.unwords . fmap fst . P.filter snd

gadget :: G.Gadget Action () (R.Gizmo Model Plan) (D.DList Command)
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
    newURL <- MaybeT (JE.fromJS' <$> JE.getProperty "newURL" evt)
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
