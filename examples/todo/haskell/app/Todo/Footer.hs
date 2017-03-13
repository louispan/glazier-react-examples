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
import qualified Glazier.React.Widget as R
import qualified JavaScript.Extras as JE
import Prelude hiding (filter)
import qualified Prelude as P
import qualified Todo.Filter as TD.Filter

data Command
    = RenderCommand (R.SuperModel Gasket Model) [JE.Property] J.JSVal

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

data Gasket = Gasket
    { _component :: R.ReactComponent
    , _onRender :: J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _fireClearCompleted :: J.Callback (J.JSVal -> IO ())
    } deriving G.Generic

makeClassyPrisms ''Action
makeClassy ''Gasket
makeClassy ''Model

mkGasket :: R.MModel Gasket Model -> F (R.Maker Action) Gasket
mkGasket mm = Gasket
    <$> R.getComponent
    <*> (R.mkRenderer mm $ const render)
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ClearCompletedAction)

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
        ]

render :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
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

gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
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
