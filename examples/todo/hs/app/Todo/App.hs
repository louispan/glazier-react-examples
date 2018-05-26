{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Semigroup.Applicative
import Data.SKey
import Esoteric
import qualified GHC.Generics as G
import Glazier.React.Event.HashChange
import Glazier.React.Framework
import qualified Glazier.React.Widget.Input as W
import qualified Glazier.React.Widget.MapPile.Glam as W
import qualified JavaScript.Extras as JE
import qualified Todo.Filter as TD
import qualified Todo.Footer as TD
import qualified Todo.Todo as TD

-- type TodosKey = Int

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

type TodoPile (mt :: Model (* -> *)) = W.DynamicCollection TD.Filter () (M.Map SKey) (ModelType mt TD.Todo)

-- | Just use map order
todoSorter :: Applicative m => () -> Specimen m TD.Todo -> Specimen m TD.Todo -> m Ordering
todoSorter _ _ _ = pure GT

todoFilterer :: MonadReactor m => TD.Filter -> Specimen m TD.Todo -> m Bool
todoFilterer ftr x = do
    Frame _ x' <- doReadIORef x
    pure $ case ftr of
        TD.All -> True
        TD.Active -> not $ TD.completed x'
        TD.Completed -> TD.completed x'

data App (mt :: Model (* -> *)) = App
    { newTodo :: J.JSString
    , todos :: TodoPile mt
    , footer :: TD.Footer
    } deriving G.Generic

makeLenses_ ''App

-- _newTodo :: Lens' (App mt) J.JSString
-- _newTodo = field @"newTodo"

-- -- | Can't use generic lens because of 'mt'
-- _todos :: Lens' (App mt) (TodoPile mt)
-- _todos = lens todos (\s a -> s { todos = a})

-- _footer :: Lens' (App mt) TD.Footer
-- _footer = field @"footer"


newtype NewTodo = NewTodo J.JSString

newTodoInput ::
    ( MonadReactor m
    , MonadJS m
    , MonadHTMLElement m
    )
    => Prototype p J.JSString m NewTodo
newTodoInput = (W.textInput eid)
    -- & magnifyPrototype (field @"newTodo")
    & _initializer %~ fi
    & _display %~ fd
  where
    eid = GadgetId "newTodo"

    fd disp s = modifySurfaceProperties
        (`DL.snoc` ("className", "new-todo")) (disp s)

    fi ini = ini
        ^*> (trigger' eid "onBlur" () >>= hdlBlur)
        ^*> (trigger eid "onKeyDown" (runMaybeT . fireKeyDownKey) id
            >>= maybe mempty hdlKeyDown)

    hdlBlur :: (MonadReactor m)
        => a -> MethodT (Scene p m J.JSString) m ()
    hdlBlur _ = readrT' $ \this@Obj{..} -> lift $ do
        doModifyIORef' self (my._model %~ J.strip)
        dirty this

    hdlKeyDown ::
        ( MonadReactor m
        , MonadHTMLElement m
        )
        => KeyDownKey -> MethodT (Scene p m J.JSString) m NewTodo
    hdlKeyDown (KeyDownKey _ key) = methodT' $ \this@Obj{..} fire ->
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> do
                me <- doReadIORef self
                let v = me ^. my._model
                    v' = J.strip v
                doModifyIORef' self $ my._model .~ J.empty
                if J.null v'
                    then pure ()
                    else fire $ NewTodo v'
                dirty this

            "Escape" -> blurRef eid this -- The onBlur handler will trim the value

            _ -> pure ()

newTodoInput' ::
    ( MonadReactor m
    , MonadJS m
    , MonadHTMLElement m
    )
    => Prototype p (App ('Spec m)) m NewTodo
newTodoInput' = magnifyPrototype _newTodo newTodoInput

toggleAll :: MonadReactor m => Prototype p (App ('Spec m)) m ()
toggleAll = mempty
    { display = \s -> do
        ps' <- lift $ ps s
        lf' eid s "input" (DL.fromList ps')
    -- , initializer = withRef eid
    --     ^*> (trigger' eid "onChange" () >>= hdlChange)
    }

  where
    ps s = traverse sequenceA
        [ ("key", pure . JE.toJSR . reactKey $ s ^. _plan)
        , ("type", pure $ "checkbox")
        , ("checked", JE.toJSR <$> (hasActiveTodos (s ^. _model._todos.W._rawPile)))
        ]

    eid = GadgetId "toggle-all"

    hasActiveTodos :: MonadReactor m => M.Map k (Specimen m TD.Todo) -> m Bool
    hasActiveTodos = fmap getAny . getAp . foldMap go
      where
        go td = Ap $ do
            td' <- doReadIORef td
            pure $ Any $ td' ^. _model.TD._completed

    -- hdlChange :: (MonadReactor m)
    --     => a -> MethodT (Scene p m (App ('Spec m))) m ()
    -- hdlChange _ = readrT' $ \this@Obj{..} -> do
    --     lift $ do
    --         doModifyIORef' self (my._model %~ not)
    --         dirty this

--         ToggleCompleteAllAction -> do
--             s <- use (todos . W.List.items)
--             let b = hasActiveTodos s
--             let acts = M.foldMapWithKey (toggleCompleteAll b) s
--             pure $ D.singleton $ SendTodosActionsCommand $ D.toList $ acts `D.snoc` W.List.RenderAction


-- | This is used by the React render callback
displayApp ::
    ( MonadReactor m
    , MonadJS m
    , MonadHTMLElement m
    )
    => FrameDisplay (App ('Spec m)) m ()
displayApp s = do
    bh "header" [("className", "header")] $ do
        bh "h1" [("key", "heading")] (txt "todos")
        display newTodoInput' s

        -- only render if there are todos
        -- let ts = s ^. _model.field @"todos"._rawPile
        -- if M.null ts
        --     then pure ()
        --     else bh "section" [ ("key", "main")
        --                             , ("className", "main")
        --                             ] $ do
                -- Complete all checkbox
                -- lf "input" [ ("key", "toggle-all")
                --                 , ("className", "toggle-all")
                --                 , ("type", "checkbox")
                --                 , ("checked", s ^. todos . W.List.items . to (JE.toJSR . not . hasActiveTodos))
                --                 , ("onChange", s ^. fireToggleCompleteAll . to JE.toJSR)
                --                 ]
                -- Render the list of todos
                -- display ??? (s ^. _model.field @"todos")

                -- Render the footer
                -- display TD.todoFooter (s ^. _model.field @"footer")





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

whenHashChange :: NativeEvent -> MaybeT IO J.JSString
whenHashChange evt = do
    hevt <- MaybeT $ pure $ toHashChangeEvent evt
    let n = newURL hevt
        (_, n') = J.breakOn "#" n
    pure n'

withHashChange :: J.JSString -> TD.Filter
withHashChange newHash =
    case newHash of
        "#/active" -> TD.Active
        "#/completed" -> TD.Completed
        _ -> TD.All
