{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.App
    ( Command(..)
    , Action(..)
    , AsAction(..)
    , Plan(..)
    , HasPlan(..)
    , Schema(..)
    , HasSchema(..)
    , Outline
    , Model
    , Widget
    , widget
    , TodosKey
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
import qualified Glazier.React.Gadgets.Property as G.Property
import qualified Glazier.React.Widget as R
import qualified Glazier.React.Widgets.Input as W.Input
import qualified Glazier.React.Widgets.List as W.List
import qualified JavaScript.Extras as JE
import qualified Todo.Todo as TD.Todo
import qualified Todo.Footer as TD.Footer
import qualified Todo.Filter as TD.Filter

type TodosKey = Int

data Command
    = RenderCommand (R.Gizmo Model Plan) [JE.Property] J.JSVal
    | SendTodosActionsCommand [W.List.Action TodosKey TD.Todo.Widget]
    | SendFooterActionCommand TD.Footer.Action
    | InputCommand G.Property.Command
    | TodosCommand (W.List.Command TodosKey TD.Todo.Widget)
    | FooterCommand TD.Footer.Command

data Action
    = ComponentRefAction J.JSVal
    | RenderAction
    | ToggleCompleteAllAction
    | InputAction W.Input.Action
    | TodosAction (W.List.Action TodosKey TD.Todo.Widget)
    | FooterAction TD.Footer.Action

data Schema t = Schema
    { _input :: R.Widget's t W.Input.Widget
    , _todos :: R.Widget's t (W.List.Widget TodosKey TD.Todo.Widget)
    , _footer :: R.Widget's t TD.Footer.Widget
    }

type Model = Schema R.GizmoType
type Outline = Schema R.OutlineType
instance R.ToOutline Model Outline where
    outline (Schema a b c) = Schema (R.outline a) (R.outline b) (R.outline c)

mkModel :: R.ReactMl () -> Outline -> F (R.Maker Action) Model
mkModel separator (Schema a b c) = Schema
    <$> (R.hoistWithAction InputAction (R.mkGizmo' W.Input.widget a))
    <*> (R.hoistWithAction TodosAction (R.mkGizmo' (W.List.widget separator TD.Todo.widget) b))
    <*> (R.hoistWithAction FooterAction (R.mkGizmo' TD.Footer.widget c))

data Plan = Plan
    { _component :: R.ReactComponent
    , _key :: J.JSString
    , _frameNum :: Int
    , _componentRef :: J.JSVal
    , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
    , _onComponentRef :: J.Callback (J.JSVal -> IO ())
    , _fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
    } deriving (G.Generic)

makeClassyPrisms ''Action
makeClassy ''Schema
makeClassy ''Plan

mkPlan :: R.ReactMl () -> R.Frame Model Plan -> F (R.Maker Action) Plan
mkPlan separator mm = Plan
    <$> R.getComponent
    <*> R.mkKey
    <*> pure 0
    <*> pure J.nullRef
    <*> (R.mkRenderer mm (const (render separator)))
    <*> (R.mkHandler $ pure . pure . ComponentRefAction)
    <*> (R.mkHandler $ pure . pure . const ToggleCompleteAllAction)

instance CD.Disposing Plan
instance CD.Disposing Model where
    disposing s = CD.DisposeList $
        CD.disposing (s ^. input)
        : CD.disposing (s ^. footer)
        : foldr ((:) . CD.disposing) [] (s ^. (todos . W.List.items))

-- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
instance HasPlan (R.Scene Model Plan) where
    plan = R.plan
instance HasSchema (R.Scene Model Plan) R.GizmoType where
    schema = R.model
instance HasPlan (R.Gizmo Model Plan) where
    plan = R.scene . plan
instance HasSchema (R.Gizmo Model Plan) R.GizmoType where
    schema = R.scene . schema

type Widget = R.Widget Action () Outline Model Plan Command
widget :: R.ReactMl () -> Widget
widget separator = R.Widget
    (mkModel separator)
    (mkPlan separator)
    window
    (gadget separator)

hasActiveTodos :: M.Map TodosKey (R.GizmoOf TD.Todo.Widget) -> Bool
hasActiveTodos = getAny . foldMap (Any . isActiveTodo . R.outline)

isActiveTodo :: (R.OutlineOf TD.Todo.Widget) -> Bool
isActiveTodo = view (TD.Todo.completed . to not)

-- | This is used by parent components to render this component
window :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
window = do
    s <- ask
    lift $ R.lf (s ^. component . to JE.toJS')
        [ ("key",  s ^. key . to JE.toJS')
        , ("render", s ^. onRender . to JE.toJS')
        , ("ref", s ^. onComponentRef . to JE.toJS')
        ]

-- | This is used by the React render callback
render :: R.ReactMlT Identity () -> G.WindowT (R.Scene Model Plan) R.ReactMl ()
render separator = do
    s <- ask
    lift $ R.bh "header" [("className", "header")] $ do
        R.bh "h1" [("key", "heading")] (R.txt "todos")
        view G._WindowT inputWindow s
        view G._WindowT (mainWindow separator) s

mainWindow :: R.ReactMlT Identity () -> G.WindowT (R.Scene Model Plan) R.ReactMl ()
mainWindow separator = do
    -- only render if there are todos
    ts <- view (todos . W.List.items)
    if null ts
        then pure ()
        else do
        s <- ask
        lift $ R.bh "section" [ ("key", "main")
                              , ("className", "main")
                              ] $ do
            -- Complete all checkbox
            R.lf "input" [ ("key", "toggle-all")
                         , ("className", "toggle-all")
                         , ("type", "checkbox")
                         , ("checked", s ^. todos . W.List.items . to (JE.toJS' . not . hasActiveTodos))
                         , ("onChange", s ^. fireToggleCompleteAll . to JE.toJS')
                         ]
            -- Render the list of todos
            view G._WindowT (todoListWindow separator) s

            -- Render the footer
            view G._WindowT footerWindow s

inputWindow :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
inputWindow = magnify (input . R.scene) (R.window W.Input.widget)

todoListWindow :: R.ReactMlT Identity () -> G.WindowT (R.Scene Model Plan) R.ReactMl ()
todoListWindow separator = magnify (todos . R.scene) (R.window (W.List.widget separator TD.Todo.widget))

footerWindow :: G.WindowT (R.Scene Model Plan) R.ReactMl ()
footerWindow = magnify (footer . R.scene) (R.window TD.Footer.widget)

updateFooterGadget :: G.Gadget Action () (R.Gizmo Model Plan) (D.DList Command)
updateFooterGadget = do
    (active, completed) <- use (todos . W.List.items . to (M.partition (isActiveTodo . R.outline)))
    pure $ D.singleton $ SendFooterActionCommand
                (TD.Footer.SetCountsAction (length active) (length completed))

gadget :: R.ReactMlT Identity () -> G.Gadget Action () (R.Gizmo Model Plan) (D.DList Command)
gadget separator = do
    a <- ask
    case a of
        ComponentRefAction node -> do
            componentRef .= node
            pure mempty

        RenderAction ->
            D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand

        ToggleCompleteAllAction -> do
            s <- use (todos . W.List.items)
            let b = hasActiveTodos s
            let acts = M.foldMapWithKey (toggleCompleteAll b) s
            pure $ D.singleton $ SendTodosActionsCommand $ D.toList $ acts `D.snoc` W.List.RenderAction

        InputAction (W.Input.SubmitAction _ str) -> do
            cmds <- inputGadget
            let str' = J.strip str
            cmds' <- if J.null str'
                then pure mempty
                else pure . D.singleton $ SendTodosActionsCommand [W.List.MakeItemAction
                                                                 (+ 1)
                                                                 (pure . toTodoModel str')
                                                             ]
            pure $ cmds `mappend` cmds'

        InputAction _ -> inputGadget

        TodosAction (W.List.ItemAction k TD.Todo.DestroyAction) -> do
            cmds <- todosGadget separator
            cmds' <- pure $ D.singleton $ SendTodosActionsCommand [W.List.DestroyItemAction k]
            pure $ cmds `mappend` cmds'

        TodosAction (W.List.DestroyItemAction _) -> do
            cmds <- todosGadget separator
            ts <- use (todos . W.List.items)
            -- if ts is now empty, we need to render app again (to hide todo list & footer)
            cmds' <- if null ts
                then D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
                else pure mempty
            cmds'' <- updateFooterGadget
            pure $ cmds `mappend` cmds' `mappend` cmds''

        TodosAction (W.List.AddItemAction _ _) -> do
            ts <- use (todos . W.List.items)
            cmds <- todosGadget separator
            -- if ts was empty, we need to render app again (to hide todo list & footer)
            cmds' <- if null ts
                then D.singleton <$> R.basicRenderCmd frameNum componentRef RenderCommand
                else pure mempty
            cmds'' <- updateFooterGadget
            pure $ cmds `mappend` cmds' `mappend` cmds''

        TodosAction (W.List.ItemAction _ TD.Todo.ToggleCompletedAction) -> do
            cmds <- todosGadget separator
            cmds' <- updateFooterGadget
            pure $ cmds `mappend` cmds' `D.snoc` SendTodosActionsCommand [W.List.RenderAction]

        TodosAction _ -> do
            cmds <- todosGadget separator
            cmds' <- updateFooterGadget
            pure $ cmds `mappend` cmds'

        FooterAction TD.Footer.ClearCompletedAction -> do
            cmds <- footerGadget
            (todos . W.List.items) %= M.filter (isActiveTodo . R.outline)
            cmds' <- updateFooterGadget
            pure $ cmds `mappend` cmds' `D.snoc` SendTodosActionsCommand [W.List.RenderAction]

        FooterAction (TD.Footer.SetFilterAction _) -> do
            cmds <- footerGadget
            ftr <- use (footer . TD.Footer.filter)
            let p = case ftr of
                    TD.Filter.All -> const True
                    TD.Filter.Active -> isActiveTodo
                    TD.Filter.Completed -> not . isActiveTodo
            pure $ cmds `D.snoc` SendTodosActionsCommand [W.List.SetFilterAction p]

        FooterAction _ -> footerGadget

  where
    toTodoModel :: J.JSString -> TodosKey -> R.ModelOf TD.Todo.Widget
    toTodoModel str _ = TD.Todo.Schema
        str
        False
        False
        False

    toggleCompleteAll
        :: Bool
        -> TodosKey
        -> R.GizmoOf TD.Todo.Widget
        -> D.DList (W.List.Action TodosKey TD.Todo.Widget)
    toggleCompleteAll b k todoGizmo =
        if todoGizmo ^. (TD.Todo.schema . TD.Todo.completed) /= b
            then D.singleton $ W.List.ItemAction k (TD.Todo.SetCompletedAction b)
            else mempty

inputGadget :: G.Gadget Action () (R.Gizmo Model Plan) (D.DList Command)
inputGadget = fmap InputCommand <$> magnify _InputAction (zoom input (W.Input.resetGadget go))
  where
    go ( W.Input.SubmitAction j _) = Just j
    go ( W.Input.CancelAction j) = Just j
    go _ = Nothing

todosGadget :: R.ReactMl () -> G.Gadget Action () (R.Gizmo Model Plan) (D.DList Command)
todosGadget separator = fmap TodosCommand <$> magnify _TodosAction (zoom todos
                                                         (R.gadget (W.List.widget separator TD.Todo.widget)))

footerGadget :: G.Gadget Action () (R.Gizmo Model Plan) (D.DList Command)
footerGadget = fmap FooterCommand <$> magnify _FooterAction (zoom footer (R.gadget TD.Footer.widget))
