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
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Command as R
import qualified Glazier.React.Component as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Widget as R
import qualified Glazier.React.Widgets.Input as W.Input
import qualified Glazier.React.Widgets.List as W.List
import qualified JavaScript.Extras as JE
import qualified Todo.Todo as TD.Todo
import qualified Todo.Footer as TD.Footer
import qualified Todo.Filter as TD.Filter

type TodosKey = Int

data Command
    = RenderCommand (R.SuperModel Gasket Model) [JE.Property] J.JSVal
    | SendActionsCommand [Action]
    | InputCommand (W.Input.Command Action)
    | TodosCommand (W.List.Command TodosKey TD.Todo.Widget)
    | FooterCommand TD.Footer.Command

data Action
    = ComponentRefAction J.JSVal
    | RenderAction
    | ToggleCompleteAllAction
    | InputAction (W.Input.Action Action)
    | TodosAction (W.List.Action TodosKey TD.Todo.Widget)
    | FooterAction TD.Footer.Action

data Model = Model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _input :: R.WidgetSuperModel (W.Input.Widget Action)
    , _todos :: R.WidgetSuperModel (W.List.Widget TodosKey TD.Todo.Widget)
    , _footer :: R.WidgetSuperModel TD.Footer.Widget
    }

data Gasket = Gasket
    { _component :: R.ReactComponent
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Gasket
makeClassy ''Model

mkGasket :: (R.MModel Gasket Model) -> F (R.Maker Action) Gasket
mkGasket mm = Gasket
    <$> R.getComponent
    <*> (R.mkRenderer mm (const render))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ToggleCompleteAllAction)

instance CD.Disposing Model where
    disposing s = CD.DisposeList $
        CD.disposing (s ^. input)
        : foldr ((:) . CD.disposing) [] (s ^. (todos . W.List.itemsModel))

mkSuperModel
    :: W.Input.Model
    -> W.List.Model TodosKey TD.Todo.Widget
    -> TD.Footer.Model
    -> (R.WidgetSuperModel (W.Input.Widget Action)
        -> R.WidgetSuperModel (W.List.Widget TodosKey TD.Todo.Widget)
        -> R.WidgetSuperModel TD.Footer.Widget
        -> Model
       )
    -> F (R.Maker Action) (R.SuperModel Gasket Model)
mkSuperModel inputModel todosModel footerModel f = do
    inputSuperModel <- hoistF (R.mapAction $ review _InputAction) $
        W.Input.mkSuperModel inputModel
    todosSuperModel <- hoistF (R.mapAction $ review _TodosAction) $
        W.List.mkSuperModel TD.Todo.window mempty todosModel
    footerSuperModel <- hoistF (R.mapAction $ review _FooterAction) $
        TD.Footer.mkSuperModel footerModel
    R.mkSuperModel mkGasket $ \gsk -> R.GModel gsk (f inputSuperModel todosSuperModel footerSuperModel)

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

hasActiveTodos :: M.Map TodosKey TD.Todo.SuperModel -> Bool
hasActiveTodos = getAny . foldMap (Any . isActiveTodo)

isActiveTodo :: TD.Todo.SuperModel -> Bool
isActiveTodo = view (TD.Todo.model . TD.Todo.completed . to not)

-- | This is used by parent components to render this component
window :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to J.pToJSVal)
        [ ("key",  s ^. uid . to J.pToJSVal)
        , ("render", s ^. onRender . to JE.PureJSVal . to J.pToJSVal)
        , ("ref", s ^. onComponentRef . to JE.PureJSVal . to J.pToJSVal)
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
    -- only render if there are todos
    ts <- view (todos . W.List.itemsModel)
    if (null ts)
        then pure ()
        else do
        s <- ask
        lift $ R.bh (JE.strval "section") [ ("key", JE.strval "main")
                                         , ("className", JE.strval "main")
                                         ] $ do
            -- Complete all checkbox
            R.lf (JE.strval "input")
                        [ ("key", JE.strval "toggle-all")
                        , ("className", JE.strval "toggle-all")
                        , ("type", JE.strval "checkbox")
                        , ("checked", s ^. todos . W.List.itemsModel . to (J.pToJSVal . not . hasActiveTodos))
                        , ("onChange", s ^. fireToggleCompleteAll . to J.jsval)
                        ]
            -- Render the list of todos
            view G._WindowT todoListWindow s

            -- Render the footer
            view G._WindowT footerWindow s

inputWindow :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
inputWindow = magnify (input . R.gModel) W.Input.window

todoListWindow :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
todoListWindow = magnify (todos . R.gModel) W.List.window

footerWindow :: Monad m => G.WindowT GModel (R.ReactMlT m) ()
footerWindow = magnify (footer . R.gModel) TD.Footer.window

updateFooterGadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
updateFooterGadget = do
    (active, completed) <- use (todos . W.List.itemsModel . to (M.partition isActiveTodo))
    pure $ D.singleton $ SendActionsCommand
                [FooterAction $ TD.Footer.SetCountsAction (length active) (length completed)]

gadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
gadget = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        RenderAction ->
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

        ToggleCompleteAllAction -> do
            s <- use (todos . W.List.itemsModel)
            let b = hasActiveTodos s
            let acts = M.foldMapWithKey (toggleCompleteAll b) s
            pure $ D.singleton $ SendActionsCommand $ D.toList $ acts

        InputAction (W.Input.SubmitAction str) -> do
            cmds <- inputGadget
            let str' = J.strip str
            cmds' <- if J.null str'
                then pure mempty
                else pure $ D.singleton $ SendActionsCommand [TodosAction $ W.List.MakeItemAction (+ 1) (toTodoModel str')]
            pure $ cmds `mappend` cmds'

        InputAction _ -> inputGadget

        TodosAction (W.List.ItemAction k TD.Todo.DestroyAction) -> do
            cmds <- todosGadget
            cmds' <- pure $ D.singleton $ SendActionsCommand [TodosAction $ W.List.DestroyItemAction k]
            pure $ cmds `mappend` cmds'

        TodosAction (W.List.DestroyItemAction _) -> do
            cmds <- todosGadget
            ts <- use (todos . W.List.itemsModel)
            -- if ts is now empty, we need to render app again (to hide todo list & footer)
            cmds' <- if (null ts)
                then D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
                else pure mempty
            cmds'' <- updateFooterGadget
            pure $ cmds `mappend` cmds' `mappend` cmds''

        TodosAction (W.List.AddItemAction _ _) -> do
            ts <- use (todos . W.List.itemsModel)
            cmds <- todosGadget
            -- if ts was empty, we need to render app again (to hide todo list & footer)
            cmds' <- if (null ts)
                then D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
                else pure mempty
            cmds'' <- updateFooterGadget
            pure $ cmds `mappend` cmds' `mappend` cmds''

        TodosAction (W.List.ItemAction _ TD.Todo.ToggleCompletedAction) -> do
            cmds <- todosGadget
            cmds' <- updateFooterGadget
            pure $ cmds `mappend` cmds' `D.snoc` SendActionsCommand [TodosAction W.List.RenderAction]

        TodosAction _ -> do
            cmds <- todosGadget
            cmds' <- updateFooterGadget
            pure $ cmds `mappend` cmds'

        FooterAction TD.Footer.ClearCompletedAction -> do
            cmds <- footerGadget
            (todos . W.List.itemsModel) %= M.filter isActiveTodo
            cmds' <- updateFooterGadget
            pure $ cmds `mappend` cmds' `D.snoc` SendActionsCommand [TodosAction W.List.RenderAction]

        FooterAction (TD.Footer.SetFilterAction _) -> do
            cmds <- footerGadget
            ftr <- use (footer . TD.Footer.filter)
            let p = case ftr of
                    TD.Filter.All -> const True
                    TD.Filter.Active -> isActiveTodo
                    TD.Filter.Completed -> not . isActiveTodo
            pure $ cmds `D.snoc` SendActionsCommand [TodosAction $ W.List.SetFilterAction p]

        FooterAction _ -> footerGadget

  where
    toTodoModel :: J.JSString -> TodosKey -> TD.Todo.Model
    toTodoModel str k = TD.Todo.Model
        (J.pack . show $ k)
        J.nullRef
        0
        mempty
        J.nullRef
        str
        False
        False

    toggleCompleteAll
        :: Bool
        -> TodosKey
        -> R.WidgetSuperModel TD.Todo.Widget
        -> D.DList Action
    toggleCompleteAll b k todoSuperModel =
        if (todoSuperModel ^. (TD.Todo.model . TD.Todo.completed) /= b)
            then D.singleton $ TodosAction $ W.List.ItemAction k (TD.Todo.SetCompletedAction b)
            else mempty

inputGadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
inputGadget = fmap InputCommand <$> magnify _InputAction (zoom input W.Input.gadget)

todosGadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
todosGadget = fmap TodosCommand <$> magnify _TodosAction (zoom todos
                                                         (W.List.gadget
                                                         TD.Todo.mkSuperModel
                                                         TD.Todo.gadget))

footerGadget :: Monad m => G.GadgetT Action SuperModel m (D.DList Command)
footerGadget = fmap FooterCommand <$> magnify _FooterAction (zoom footer TD.Footer.gadget)
