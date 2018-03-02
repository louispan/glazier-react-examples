{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.App where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Diverse.Profunctor
import Data.Generics.Product
import qualified Data.JSString as J
import Data.Maybe
import qualified GHC.Generics as G
import qualified Glazier.React.Framework as Z
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD
import qualified Todo.Footer as TD
import qualified Todo.Todo as TD

-- type TodosKey = Int

-- data Command
--     = RenderCommand (Z.Gizmo Model Plan) [JE.Property] J.JSVal
--     | SendTodosActionsCommand [W.List.Action TodosKey TD.Todo.Widget]
--     | SendFooterActionCommand TD.Footer.Action
--     | InputCommand G.Property.Command
--     | TodosCommand (W.List.Command TodosKey TD.Todo.Widget)
--     | FooterCommand TD.Footer.Command

-- data Action
--     = ComponentRefAction J.JSVal
--     | RenderAction
--     | ToggleCompleteAllAction
--     | InputAction W.Input.Action
--     | TodosAction (W.List.Action TodosKey TD.Todo.Widget)
--     | FooterAction TD.Footer.Action

data App = App
    { input :: J.JSString
    -- , todos :: Z.Widget's t (W.List.Widget TodosKey TD.Todo.Widget)
    , footer :: TD.TodoFooter
    } deriving G.Generic

-- type Model = Schema Z.GizmoType
-- type Outline = Schema Z.OutlineType
-- instance Z.ToOutline Model Outline where
--     outline (Schema a b c) = Schema (Z.outline a) (Z.outline b) (Z.outline c)

-- mkModel :: Z.ReactMl () -> Outline -> F (Z.Maker Action) Model
-- mkModel separator (Schema a b c) = Schema
--     <$> (Z.hoistWithAction InputAction (Z.mkGizmo' W.Input.widget a))
--     <*> (Z.hoistWithAction TodosAction (Z.mkGizmo' (W.List.widget separator TD.Todo.widget) b))
--     <*> (Z.hoistWithAction FooterAction (Z.mkGizmo' TD.Footer.widget c))

-- data Plan = Plan
--     { _component :: Z.ReactComponent
--     , _key :: J.JSString
--     , _frameNum :: Int
--     , _componentRef :: J.JSVal
--     , _onRender ::  J.Callback (J.JSVal -> IO J.JSVal)
--     , _onComponentRef :: J.Callback (J.JSVal -> IO ())
--     , _fireToggleCompleteAll :: J.Callback (J.JSVal -> IO ())
--     } deriving (G.Generic)

-- makeClassyPrisms ''Action
-- makeClassy ''Schema
-- makeClassy ''Plan

-- mkPlan :: Z.ReactMl () -> Z.Frame Model Plan -> F (Z.Maker Action) Plan
-- mkPlan separator mm = Plan
--     <$> Z.getComponent
--     <*> Z.mkKey
--     <*> pure 0
--     <*> pure J.nullRef
--     <*> (Z.mkRenderer mm (const (render separator)))
--     <*> (Z.mkHandler $ pure . pure . ComponentRefAction)
--     <*> (Z.mkHandler $ pure . pure . const ToggleCompleteAllAction)

-- instance CD.Disposing Plan
-- instance CD.Disposing Model where
--     disposing s = CD.DisposeList $
--         CD.disposing (s ^. input)
--         : CD.disposing (s ^. footer)
--         : foldr ((:) . CD.disposing) [] (s ^. (todos . W.List.items))

-- -- Link Glazier.React.Model's HasPlan/HasModel with this widget's HasPlan/HasModel from makeClassy
-- instance HasPlan (Z.Scene Model Plan) where
--     plan = Z.plan
-- instance HasSchema (Z.Scene Model Plan) Z.GizmoType where
--     schema = Z.model
-- instance HasPlan (Z.Gizmo Model Plan) where
--     plan = Z.scene . plan
-- instance HasSchema (Z.Gizmo Model Plan) Z.GizmoType where
--     schema = Z.scene . schema

-- type Widget = Z.Widget Action Outline Model Plan Command
-- widget :: Z.ReactMl () -> Widget
-- widget separator = Z.Widget
--     (mkModel separator)
--     (mkPlan separator)
--     window
--     (gadget separator)

-- hasActiveTodos :: M.Map TodosKey (Z.GizmoOf TD.Todo.Widget) -> Bool
-- hasActiveTodos = getAny . foldMap (Any . isActiveTodo . Z.outline)

-- isActiveTodo :: (Z.OutlineOf TD.Todo.Widget) -> Bool
-- isActiveTodo = view (TD.Todo.completed . to not)

-- -- | This is used by parent components to render this component
-- window :: G.WindowT (Z.Scene Model Plan) Z.ReactMl ()
-- window = do
--     s <- ask
--     lift $ Z.lf (s ^. component . to JE.toJSR)
--         [ ("key",  s ^. key . to JE.toJSR)
--         , ("render", s ^. onRender . to JE.toJSR)
--         , ("ref", s ^. onComponentRef . to JE.toJSR)
--         ]

-- -- | This is used by the React render callback
-- render :: Z.ReactMlT Identity () -> G.WindowT (Z.Scene Model Plan) Z.ReactMl ()
-- render separator = do
--     s <- ask
--     lift $ Z.bh "header" [("className", "header")] $ do
--         Z.bh "h1" [("key", "heading")] (Z.txt "todos")
--         view G._WindowT inputWindow s
--         view G._WindowT (mainWindow separator) s

-- mainWindow :: Z.ReactMlT Identity () -> G.WindowT (Z.Scene Model Plan) Z.ReactMl ()
-- mainWindow separator = do
--     -- only render if there are todos
--     ts <- view (todos . W.List.items)
--     if null ts
--         then pure ()
--         else do
--         s <- ask
--         lift $ Z.bh "section" [ ("key", "main")
--                               , ("className", "main")
--                               ] $ do
--             -- Complete all checkbox
--             Z.lf "input" [ ("key", "toggle-all")
--                          , ("className", "toggle-all")
--                          , ("type", "checkbox")
--                          , ("checked", s ^. todos . W.List.items . to (JE.toJSR . not . hasActiveTodos))
--                          , ("onChange", s ^. fireToggleCompleteAll . to JE.toJSR)
--                          ]
--             -- Render the list of todos
--             view G._WindowT (todoListWindow separator) s

--             -- Render the footer
--             view G._WindowT footerWindow s

-- inputWindow :: G.WindowT (Z.Scene Model Plan) Z.ReactMl ()
-- inputWindow = magnify (input . Z.scene) (Z.window W.Input.widget)

-- todoListWindow :: Z.ReactMlT Identity () -> G.WindowT (Z.Scene Model Plan) Z.ReactMl ()
-- todoListWindow separator = magnify (todos . Z.scene) (Z.window (W.List.widget separator TD.Todo.widget))

-- footerWindow :: G.WindowT (Z.Scene Model Plan) Z.ReactMl ()
-- footerWindow = magnify (footer . Z.scene) (Z.window TD.Footer.widget)

-- updateFooterGadget :: G.Gadget Action (Z.Gizmo Model Plan) (D.DList Command)
-- updateFooterGadget = do
--     (active, completed) <- use (todos . W.List.items . to (M.partition (isActiveTodo . Z.outline)))
--     pure $ D.singleton $ SendFooterActionCommand
--                 (TD.Footer.SetCountsAction (length active) (length completed))

-- gadget :: Z.ReactMlT Identity () -> G.Gadget Action (Z.Gizmo Model Plan) (D.DList Command)
-- gadget separator = do
--     a <- ask
--     case a of
--         ComponentRefAction node -> do
--             componentRef .= node
--             pure mempty

--         RenderAction ->
--             D.singleton <$> Z.basicRenderCmd frameNum componentRef RenderCommand

--         ToggleCompleteAllAction -> do
--             s <- use (todos . W.List.items)
--             let b = hasActiveTodos s
--             let acts = M.foldMapWithKey (toggleCompleteAll b) s
--             pure $ D.singleton $ SendTodosActionsCommand $ D.toList $ acts `D.snoc` W.List.RenderAction

--         InputAction (W.Input.SubmitAction _ str) -> do
--             cmds <- inputGadget
--             let str' = J.strip str
--             cmds' <- if J.null str'
--                 then pure mempty
--                 else pure . D.singleton $ SendTodosActionsCommand [W.List.MakeItemAction
--                                                                  (+ 1)
--                                                                  (pure . toTodoModel str')
--                                                              ]
--             pure $ cmds `mappend` cmds'

--         InputAction _ -> inputGadget

--         TodosAction (W.List.ItemAction k TD.Todo.DestroyAction) -> do
--             cmds <- todosGadget separator
--             cmds' <- pure $ D.singleton $ SendTodosActionsCommand [W.List.DestroyItemAction k]
--             pure $ cmds `mappend` cmds'

--         TodosAction (W.List.DestroyItemAction _) -> do
--             cmds <- todosGadget separator
--             ts <- use (todos . W.List.items)
--             -- if ts is now empty, we need to render app again (to hide todo list & footer)
--             cmds' <- if null ts
--                 then D.singleton <$> Z.basicRenderCmd frameNum componentRef RenderCommand
--                 else pure mempty
--             cmds'' <- updateFooterGadget
--             pure $ cmds `mappend` cmds' `mappend` cmds''

--         TodosAction (W.List.AddItemAction _ _) -> do
--             ts <- use (todos . W.List.items)
--             cmds <- todosGadget separator
--             -- if ts was empty, we need to render app again (to hide todo list & footer)
--             cmds' <- if null ts
--                 then D.singleton <$> Z.basicRenderCmd frameNum componentRef RenderCommand
--                 else pure mempty
--             cmds'' <- updateFooterGadget
--             pure $ cmds `mappend` cmds' `mappend` cmds''

--         TodosAction (W.List.ItemAction _ TD.Todo.ToggleCompletedAction) -> do
--             cmds <- todosGadget separator
--             cmds' <- updateFooterGadget
--             pure $ cmds `mappend` cmds' `D.snoc` SendTodosActionsCommand [W.List.RenderAction]

--         TodosAction _ -> do
--             cmds <- todosGadget separator
--             cmds' <- updateFooterGadget
--             pure $ cmds `mappend` cmds'

--         FooterAction TD.Footer.ClearCompletedAction -> do
--             cmds <- footerGadget
--             (todos . W.List.items) %= M.filter (isActiveTodo . Z.outline)
--             cmds' <- updateFooterGadget
--             pure $ cmds `mappend` cmds' `D.snoc` SendTodosActionsCommand [W.List.RenderAction]

--         FooterAction (TD.Footer.SetFilterAction _) -> do
--             cmds <- footerGadget
--             ftr <- use (footer . TD.Footer.filter)
--             let p = case ftr of
--                     TD.Filter.All -> const True
--                     TD.Filter.Active -> isActiveTodo
--                     TD.Filter.Completed -> not . isActiveTodo
--             pure $ cmds `D.snoc` SendTodosActionsCommand [W.List.SetFilterAction p]

--         FooterAction _ -> footerGadget

--   where
--     toTodoModel :: J.JSString -> TodosKey -> Z.ModelOf TD.Todo.Widget
--     toTodoModel str _ = TD.Todo.Schema
--         str
--         False
--         False
--         False

--     toggleCompleteAll
--         :: Bool
--         -> TodosKey
--         -> Z.GizmoOf TD.Todo.Widget
--         -> D.DList (W.List.Action TodosKey TD.Todo.Widget)
--     toggleCompleteAll b k todoGizmo =
--         if todoGizmo ^. (TD.Todo.schema . TD.Todo.completed) /= b
--             then D.singleton $ W.List.ItemAction k (TD.Todo.SetCompletedAction b)
--             else mempty

-- inputGadget :: G.Gadget Action (Z.Gizmo Model Plan) (D.DList Command)
-- inputGadget = fmap InputCommand <$> magnify _InputAction (zoom input (W.Input.resetGadget go))
--   where
--     go ( W.Input.SubmitAction j _) = Just j
--     go ( W.Input.CancelAction j) = Just j
--     go _ = Nothing

-- todosGadget :: Z.ReactMl () -> G.Gadget Action (Z.Gizmo Model Plan) (D.DList Command)
-- todosGadget separator = fmap TodosCommand <$> magnify _TodosAction (zoom todos
--                                                          (Z.gadget (W.List.widget separator TD.Todo.widget)))

-- footerGadget :: G.Gadget Action (Z.Gizmo Model Plan) (D.DList Command)
-- footerGadget = fmap FooterCommand <$> magnify _FooterAction (zoom footer (Z.gadget TD.Footer.widget))
