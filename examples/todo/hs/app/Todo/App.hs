-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.App where

import Control.Lens
import Control.Lens.Misc
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Proxy
import Data.Semigroup.Applicative
import qualified GHC.Generics as G
import Glazier.React
import Glazier.React.Action.KeyDownKey
import Glazier.React.Effect.HTMLElement
import Glazier.React.Effect.JavaScript
import qualified Glazier.React.Widgets.Collection.Dynamic as W
import qualified Glazier.React.Widgets.Input as W
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD
import qualified Todo.Footer as TD
import qualified Todo.Todo as TD

-- data Command
--     = RenderCommand (Element Model Plan) [JE.Property] J.JSVal
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

type TodoCollection f = W.DynamicCollection TD.Filter () W.UKey TD.Todo f

-- | Just use map order
todoSorter :: Applicative m => a -> a -> m Ordering
todoSorter _ _ = pure LT

todoFilterer :: TD.Filter -> Subject TD.Todo -> ReadIORef Bool
todoFilterer ftr sbj = do
    scn <- doReadIORef $ sceneRef sbj
    pure $ case ftr of
        TD.All -> True
        TD.Active -> not $ TD.completed $ model scn
        TD.Completed -> TD.completed $ model scn

data App f = App
    { newTodo :: J.JSString
    , todos :: TodoCollection f
    , footer :: TD.Footer
    } deriving (G.Generic)

makeLenses_ ''App

newtype NewTodo = NewTodo J.JSString

--   where
--     ri = GadgetId "newTodo"

newTodoInput :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => ReactId -> Widget cmd p J.JSString NewTodo
newTodoInput ri =
    let wid = finish . void . overWindow fw $ W.textInput ri
    in wid `also` lift gad
  where
    fw = (*> modify' (overSurfaceProperties (`DL.snoc` ("className", "new-todo"))))
    gad = (finish $ trigger_ ri _always "onBlur" () *> hdlBlur)
        `also` (trigger' ri _always "onKeyDown" fireKeyDownKey
            >>= hdlKeyDown)

    hdlBlur :: AsReactor cmd => Gadget cmd p J.JSString ()
    hdlBlur = tickScene $ _model %= J.strip

    hdlKeyDown :: (AsReactor cmd, AsHTMLElement cmd) => KeyDownKey -> Gadget cmd p J.JSString NewTodo
    hdlKeyDown (KeyDownKey _ key) =
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> tickSceneThen $ do
                v <- use _model
                let v' = J.strip v
                _model .= J.empty
                pure $ if J.null v'
                    then finish $ pure ()
                    else pure $ NewTodo v'

            "Escape" -> finish $ blurElement ri -- The onBlur handler will trim the value

            _ -> finish $ pure ()

-- newTodoInput' ::
--     ( MonadReactor m
--     , MonadJS m
--     , MonadHTMLElement m
--     )
--     => Prototype p (App ('Spec m)) m NewTodo
-- newTodoInput' = magnifyPrototype _newTodo newTodoInput
    -- & enlargeModel _newTodo

--     ri = GadgetId "toggle-all"

completeAll :: (AsReactor cmd)
    => ReactId -> Widget cmd p (TodoCollection Subject) a
completeAll ri =
    let win = do
            scn <- ask
            ps' <- lift $ ps scn
            lf' ri "input" (DL.fromList ps')
        gad = finish $ hdlElementalRef ri `also` hdlChange
    in (display win) `also` (lift gad)
  where
    ps :: Scene (TodoCollection Subject) -> ReadIORef [JE.Property]
    ps scn = traverse sequenceA
        [ ("key", pure . JE.toJSR $ ri)
        , ("type", pure $ "checkbox")
        , ("checked", JE.toJSR <$> (hasActiveTodos (scn ^. _model.W._rawCollection)))
        ]

    hasActiveTodos :: M.Map k (Subject TD.Todo) -> ReadIORef Bool
    hasActiveTodos = fmap getAny . getAp . foldMap go
      where
        go sbj = Ap $ do
            scn <- doReadIORef $ sceneRef sbj
            pure $ Any $ scn ^. _model.TD._completed

    hdlChange :: AsReactor cmd => Gadget cmd p (TodoCollection Subject) ()
    hdlChange = do
        trigger_ ri _always "onChange" ()
        scn <- getScene
        getAls $ foldMap (\sbj -> Als $ lift $ gadgetWith sbj
                (tickScene $ (_model.TD._completed) .= True))
            (view (_model.W._rawCollection) scn)

-- -- | This is used by the React render callback
-- displayApp :: forall cmd. (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
--     => Proxy cmd -> ElementalId -> ElementalId -> Window (App Subject) ()
-- displayApp _ newTodoId completeAllId = do
--     s <- ask
--     bh "header" [("className", "header")] $ do
--         bh "h1" [("key", "heading")] (txt "todos")
--         enlargeScene _newTodo (window (newTodoInput @cmd newTodoId))

--         -- only render if there are todos
--         let ts = s ^. _model._todos.W._rawCollection
--         if M.null ts
--             then pure ()
--             else bh "section" [ ("key", "main")
--                                     , ("className", "main")
--                                     ] $ do
--                 -- Complete all checkbox
--                 enlargeScene _todos (window (completeAll @cmd completeAllId))

--                 -- -- Render the list of todos
--                 -- display ??? (s ^. _model.field @"todos")

--                 -- Render the footer
--                 display TD.todoFooter (s ^. _model.field @"footer")





-- inputWindow :: G.WindowT (Scene Model Plan) ReactMl ()
-- inputWindow = magnify (input . scene) (window W.Input.widget)

-- todoListWindow :: ReactMlT Identity () -> G.WindowT (Scene Model Plan) ReactMl ()
-- todoListWindow separator = magnify (todos . scene) (window (W.List.widget separator TD.Todo.widget))

-- footerWindow :: G.WindowT (Scene Model Plan) ReactMl ()
-- footerWindow = magnify (footer . scene) (window TD.Footer.widget)

-- updateFooterGadget :: G.Gadget Action (Element Model Plan) (D.DList Command)
-- updateFooterGadget = do
--     (active, completed) <- use (todos . W.List.items . to (M.partition (isActiveTodo . outline)))
--     pure $ D.singleton $ SendFooterActionCommand
--                 (TD.Footer.SetCountsAction (length active) (length completed))

-- gadget :: ReactMlT Identity () -> G.Gadget Action (Element Model Plan) (D.DList Command)
-- gadget separator = do
--     a <- ask
--     case a of
--         ComponentRefAction node -> do
--             componentRef .= node
--             pure mempty

--         RenderAction ->
--             D.singleton <$> basicRenderCmd frameNum componentRef RenderCommand

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
--                 then D.singleton <$> basicRenderCmd frameNum componentRef RenderCommand
--                 else pure mempty
--             cmds'' <- updateFooterGadget
--             pure $ cmds `mappend` cmds' `mappend` cmds''

--         TodosAction (W.List.AddItemAction _ _) -> do
--             ts <- use (todos . W.List.items)
--             cmds <- todosGadget separator
--             -- if ts was empty, we need to render app again (to hide todo list & footer)
--             cmds' <- if null ts
--                 then D.singleton <$> basicRenderCmd frameNum componentRef RenderCommand
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
--             (todos . W.List.items) %= M.filter (isActiveTodo . outline)
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
--     toTodoModel :: J.JSString -> TodosKey -> ModelOf TD.Todo.Widget
--     toTodoModel str _ = TD.Todo.Schema
--         str
--         False
--         False
--         False

--     toggleCompleteAll
--         :: Bool
--         -> TodosKey
--         -> ElementOf TD.Todo.Widget
--         -> D.DList (W.List.Action TodosKey TD.Todo.Widget)
--     toggleCompleteAll b k todoElement =
--         if todoElement ^. (TD.Todo.schema . TD.Todo.completed) /= b
--             then D.singleton $ W.List.ItemAction k (TD.Todo.SetCompletedAction b)
--             else mempty

-- inputGadget :: G.Gadget Action (Element Model Plan) (D.DList Command)
-- inputGadget = fmap InputCommand <$> magnify _InputAction (zoom input (W.Input.resetGadget go))
--   where
--     go ( W.Input.SubmitAction j _) = Just j
--     go ( W.Input.CancelAction j) = Just j
--     go _ = Nothing

-- todosGadget :: ReactMl () -> G.Gadget Action (Element Model Plan) (D.DList Command)
-- todosGadget separator = fmap TodosCommand <$> magnify _TodosAction (zoom todos
--                                                          (gadget (W.List.widget separator TD.Todo.widget)))

-- footerGadget :: G.Gadget Action (Element Model Plan) (D.DList Command)
-- footerGadget = fmap FooterCommand <$> magnify _FooterAction (zoom footer (gadget TD.Footer.widget))


-- -- | This needs to be explictly registered by the Main app
-- onHashChange ::  NativeEvent -> MaybeT IO [Action]
-- onHashChange = eventHandlerM whenHashChange withHashChange

-- whenHashChange :: NativeEvent -> MaybeT IO J.JSString
-- whenHashChange evt = do
--     hevt <- MaybeT $ pure $ toHashChangeEvent evt
--     let n = newURL hevt
--         (_, n') = J.breakOn "#" n
--     pure n'

-- withHashChange :: J.JSString -> TD.Filter
-- withHashChange newHash =
--     case newHash of
--         "#/active" -> TD.Active
--         "#/completed" -> TD.Completed
--         _ -> TD.All

