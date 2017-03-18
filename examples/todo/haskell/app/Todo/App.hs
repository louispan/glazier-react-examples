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
import Control.Monad.Morph
import Control.Monad.Reader
import qualified Data.DList as D
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified GHC.Generics as G
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Command as R
import qualified Glazier.React.Component as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified Glazier.React.Widget as R
import qualified Glazier.React.Widgets.Input as W.Input
import qualified Glazier.React.Widgets.List as W.List
import qualified JavaScript.Extras as JE
import qualified Todo.Todo as TD.Todo
import qualified Todo.Footer as TD.Footer
import qualified Todo.Filter as TD.Filter

type TodosKey = Int

data Command
    = RenderCommand (R.SuperModel Model Plan) [JE.Property] J.JSVal
    | SendActionsCommand [Action]
    | InputCommand W.Input.Command
    | TodosCommand (W.List.Command TodosKey TD.Todo.Widget)
    | FooterCommand TD.Footer.Command

data Action
    = ComponentRefAction J.JSVal
    | RenderAction
    | ToggleCompleteAllAction
    | InputAction W.Input.Action
    | TodosAction (W.List.Action TodosKey TD.Todo.Widget)
    | FooterAction TD.Footer.Action


data Model = Model
    { _uid :: J.JSString
    , _componentRef :: J.JSVal
    , _frameNum :: Int
    , _input :: R.SuperModelOf W.Input.Widget
    , _todos :: R.SuperModelOf (W.List.Widget TodosKey TD.Todo.Widget)
    , _footer :: R.SuperModelOf TD.Footer.Widget
    }

data Plan = Plan
    { _component :: R.ReactComponent
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Model
makeClassy ''Plan

mkPlan :: R.Frame Model Plan -> F (R.Maker Action) Plan
mkPlan mm = Plan
    <$> R.getComponent
    <*> (R.mkRenderer mm (const render))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ToggleCompleteAllAction)

instance CD.Disposing Plan
instance CD.Disposing Model where
    disposing s = CD.DisposeList $
        CD.disposing (s ^. input)
        : foldr ((:) . CD.disposing) [] (s ^. (todos . W.List.itemsModel))

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

----------------------------------------------------------

type Widget = R.Widget Command Action Model Plan
widget :: R.Widget Command Action Model Plan
widget = R.Widget
    mkPlan
    window
    gadget

hasActiveTodos :: M.Map TodosKey (R.SuperModelOf TD.Todo.Widget) -> Bool
hasActiveTodos = getAny . foldMap (Any . isActiveTodo)

isActiveTodo :: (R.SuperModelOf TD.Todo.Widget) -> Bool
isActiveTodo = view (TD.Todo.model . TD.Todo.completed . to not)

-- | This is used by parent components to render this component
window :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS)
        [ ("key",  s ^. uid . to JE.toJS)
        , ("render", s ^. onRender . to JE.toJS)
        , ("ref", s ^. onComponentRef . to JE.toJS)
        ]

-- | This is used by the React render callback
render :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
render = do
    s <- ask
    lift $ R.bh (JE.strJS "header") [("className", JE.strJS "header")] $ do
        R.bh (JE.strJS "h1") [("key", JE.strJS "heading")] (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT mainWindow s

mainWindow :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
mainWindow = do
    -- only render if there are todos
    ts <- view (todos . W.List.itemsModel)
    if null ts
        then pure ()
        else do
        s <- ask
        lift $ R.bh (JE.strJS "section") [ ("key", JE.strJS "main")
                                         , ("className", JE.strJS "main")
                                         ] $ do
            -- Complete all checkbox
            R.lf (JE.strJS "input")
                        [ ("key", JE.strJS "toggle-all")
                        , ("className", JE.strJS "toggle-all")
                        , ("type", JE.strJS "checkbox")
                        , ("checked", s ^. todos . W.List.itemsModel . to (JE.toJS . not . hasActiveTodos))
                        , ("onChange", s ^. fireToggleCompleteAll . to JE.toJS)
                        ]
            -- Render the list of todos
            view G._WindowT todoListWindow s

            -- Render the footer
            view G._WindowT footerWindow s

inputWindow :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
inputWindow = magnify (input . R.design) W.Input.window

todoListWindow :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
todoListWindow = magnify (todos . R.design) W.List.window

footerWindow :: G.WindowT (R.Design Model Plan) (R.ReactMlT Identity) ()
footerWindow = magnify (footer . R.design) TD.Footer.window

updateFooterGadget :: G.GadgetT Action (R.SuperModel Model Plan) Identity (D.DList Command)
updateFooterGadget = do
    (active, completed) <- use (todos . W.List.itemsModel . to (M.partition isActiveTodo))
    pure $ D.singleton $ SendActionsCommand
                [FooterAction $ TD.Footer.SetCountsAction (length active) (length completed)]

gadget :: G.GadgetT Action (R.SuperModel Model Plan) Identity (D.DList Command)
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
                else pure . D.singleton $ SendActionsCommand [TodosAction $ W.List.MakeItemAction
                                                                 (+ 1)
                                                                 (pure . toTodoModel str')
                                                             ]
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
        -> R.SuperModelOf TD.Todo.Widget
        -> D.DList Action
    toggleCompleteAll b k todoSuperModel =
        if (todoSuperModel ^. (TD.Todo.model . TD.Todo.completed) /= b)
            then D.singleton $ TodosAction $ W.List.ItemAction k (TD.Todo.SetCompletedAction b)
            else mempty

inputGadget :: G.GadgetT Action (R.SuperModel Model Plan) Identity (D.DList Command)
inputGadget = fmap InputCommand <$> magnify _InputAction (zoom input W.Input.gadget)

todosGadget :: G.GadgetT Action (R.SuperModel Model Plan) Identity (D.DList Command)
todosGadget = fmap TodosCommand <$> magnify _TodosAction (zoom todos
                                                         (W.List.gadget
                                                         (R.mkSuperModel TD.Todo.widget)
                                                         TD.Todo.gadget))

footerGadget :: G.GadgetT Action (R.SuperModel Model Plan) Identity (D.DList Command)
footerGadget = fmap FooterCommand <$> magnify _FooterAction (zoom footer TD.Footer.gadget)
