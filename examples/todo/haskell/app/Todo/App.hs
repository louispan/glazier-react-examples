{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.App
    ( TodosKey
    , TodosValue
    , TodosCommand'
    , TodosAction'
    , TodosModel'
    , Command(..)
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
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Widget as R
import qualified Glazier.React.Widgets.Input as W.Input
-- import qualified Glazier.React.Widgets.List as W.List
import qualified JavaScript.Extras as JE
import qualified Todo.Todo as TD.Todo

type TodosKey = Int

type TodosValue = R.WidgetSuperModel TD.Todo.Widget

type TodosModel' = M.Map TodosKey TodosValue

type TodosCommand' = (TodosKey, TD.Todo.Command)

type TodosAction' = (TodosKey, TD.Todo.Action)

data Command
    -- Common widget commands
    -- | This should result in React component @setState({ frameNum: i })@
    = RenderCommand (R.SuperModel Gasket Model) [JE.Property] J.JSVal

    -- General Application level commands
    -- | DisposeCommand should run dispose on the SomeDisposable (eg. to release Gasket)
    | DisposeCommand CD.SomeDisposable
    -- | Runs maker widgets creations
    | MakerCommand (F (R.Maker Action) Action)
    | SendActionsCommand [Action]

    -- TodoMVC specific commands
    | InputCommand (W.Input.Command Action)
    | TodosCommand TodosCommand'

data Action
    -- Common widget actions
    = ComponentRefAction J.JSVal
    | ComponentDidUpdateAction
    -- TodoMVC specific actions
    | ToggleCompleteAllAction
    | DestroyTodoAction TodosKey
    | RequestNewTodoAction J.JSString
    | AddNewTodoAction TodosKey TodosValue
    | InputAction (W.Input.Action Action)
    | TodosAction TodosAction'

data Model = Model
    -- Common widget model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _deferredCommands :: D.DList Command
    -- TodoMVC specifc model
    , _todoSeqNum :: TodosKey
    , _todoInput :: R.WidgetSuperModel (W.Input.Widget Action)
    , _todosModel :: TodosModel'
    }

data Gasket = Gasket
    -- Common widget callbacks
    { _component :: R.ReactComponent
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _onComponentDidUpdate :: J.Callback (J.JSVal -> IO ())
    -- TodoMVC specific callbacks
    , _fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Gasket
makeClassy ''Model

mkGasket :: (R.MModel Gasket Model) -> F (R.Maker Action) Gasket
mkGasket mm = Gasket
    -- common widget callbacks
    <$> R.getComponent
    <*> (R.mkRenderer mm (const render))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ComponentDidUpdateAction)
    -- widget specific callbacks
    <*> (R.mkHandler $ pure . pure . const ToggleCompleteAllAction)

instance CD.Disposing Model where
    disposing s = CD.DisposeList $
        CD.disposing (s ^. todoInput)
        : foldr ((:) . CD.disposing) [] (s ^. todosModel)

mkSuperModel
    :: W.Input.Model
    -> (R.WidgetSuperModel (W.Input.Widget Action) -> Model)
    -> F (R.Maker Action) (R.SuperModel Gasket Model)
mkSuperModel inputModel f = do
    inputSuperModel <- hoistF (R.mapAction $ review _InputAction) $
        W.Input.mkSuperModel $ inputModel
    R.mkSuperModel mkGasket $ \gsk -> R.GModel gsk (f inputSuperModel)

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

hasActiveTodos :: TodosModel' -> Bool
hasActiveTodos = getAny . foldMap (view (TD.Todo.model . TD.Todo.completed . to not . to Any))

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

-- | This is used by the React render callback
render :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
render = do
    s <- ask
    lift $ R.bh (JE.strval "header") [("className", JE.strval "header")] $ do
        R.bh (JE.strval "h1") [("key", JE.strval "heading")] (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT mainWindow s

mainWindow :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
mainWindow = do
    todos <- view todosModel
    if (null todos)
        then pure ()
        else do
        s <- ask
        lift $ R.bh (JE.strval "section") [ ("key", JE.strval "main")
                                         , ("className", JE.strval "main")
                                         ] $ do
            -- This is the complete all checkbox
            R.lf (JE.strval "input")
                        [ ("key", JE.strval "toggle-all")
                        , ("className", JE.strval "toggle-all")
                        , ("type", JE.strval "checkbox")
                        , ("checked", s ^. todosModel . to (J.pToJSVal . not . hasActiveTodos))
                        , ("onChange", s ^. fireToggleCompleteAll . to J.jsval)
                        ]
            view G._WindowT todoListWindow s

todoListWindow :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
todoListWindow = do
    todos <- fmap (view R.gModel . snd) . M.toList <$> view todosModel
    lift $ R.bh (JE.strval "ul") [ ("key", JE.strval "todo-list")
                                , ("className", JE.strval "todo-list")
                                ] $
        traverse_ (view G._WindowT TD.Todo.window) todos

gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
gadget = appGadget
    <> inputGadget
    <> todosGadget'

appGadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
appGadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        ComponentDidUpdateAction -> do
            -- Run delayed commands that need to wait until frame is re-rendered
            -- Eg focusing after other rendering changes
            cmds <- use deferredCommands
            deferredCommands .= mempty
            pure cmds

        ToggleCompleteAllAction -> do
            s <- use todosModel
            let b = hasActiveTodos s
            let acts = M.foldMapWithKey (toggleCompleteAll b) s
            pure $ D.singleton $ SendActionsCommand (D.toList acts)

        DestroyTodoAction k -> do
            -- queue up callbacks to be released after rerendering
            ts <- use todosModel
            ret <- runMaybeT $ do
                todoSuperModel <- MaybeT $ pure $ M.lookup k ts
                let junk = CD.disposing todoSuperModel
                deferredCommands %= (`D.snoc` DisposeCommand junk)
                -- Remove the todo from the model
                todosModel %= M.delete k
                -- on re-render the todo Shim will not get rendered and will be removed by react
                lift $ D.singleton <$> renderCmd
            maybe (pure mempty) pure ret

        RequestNewTodoAction str -> do
            n <- use todoSeqNum
            todoSeqNum %= (+ 1)
            pure $ D.singleton $ MakerCommand $ do
                ms <- hoistF (R.mapAction $ \act -> TodosAction (n, act)) $
                    TD.Todo.mkSuperModel $ TD.Todo.Model
                        (J.pack . show $ n)
                        J.nullRef
                        0
                        mempty
                        J.nullRef
                        str
                        False
                        False
                pure $ AddNewTodoAction n ms

        AddNewTodoAction n v -> do
            todosModel %= M.insert n v
            D.singleton <$> renderCmd

        -- these will be handled by monoidally appending other gadgets
        InputAction _ -> pure mempty
        TodosAction _ -> pure mempty
  where
    toggleCompleteAll
        :: Bool
        -> TodosKey
        -> R.WidgetSuperModel TD.Todo.Widget
        -> D.DList Action
    toggleCompleteAll b k todoSuperModel =
        if (todoSuperModel ^. (TD.Todo.model . TD.Todo.completed) /= b)
            then D.singleton $ TodosAction (k, TD.Todo.SetCompletedAction b)
            else mempty

inputWindow :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
inputWindow = magnify (todoInput . R.gModel) W.Input.window

inputGadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
inputGadget = fmap InputCommand <$> zoom todoInput (magnify _InputAction W.Input.gadget)

todosGadget :: Monad m => G.GadgetT TodosAction' TodosModel' m (D.DList TodosCommand')
todosGadget = do
    -- expect a (key, action) pair
    (k, a) <- ask
    x <- get
    case M.lookup k x of
        Nothing -> pure mempty
        Just sm -> do
            -- run the todo gadget logic
            (cmds, sm') <- lift $ view G._GadgetT TD.Todo.gadget a sm
            -- replace the todo state in the map
            put $ M.insert k sm' x
            -- annotate cmd with the key
            pure $ (\cmd -> (k, cmd)) <$> cmds

todosGadget' :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
todosGadget' = fmap TodosCommand <$> zoom todosModel (magnify _TodosAction todosGadget)

-- | Just change the state to something different so the React pureComponent will call render()
renderCmd :: Monad m => G.GadgetT Action SuperModel m Command
renderCmd = do
    frameNum %= (\i -> (i + 1) `mod` 100)
    i <- J.pToJSVal <$> use frameNum
    r <- use componentRef
    sm <- get
    pure $ RenderCommand sm [("frameNum", i)] r
