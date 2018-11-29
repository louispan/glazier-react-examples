{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# LANGUAGE ViewPatterns #-}

module Todo.App where

import Control.Lens
import Control.Lens.Misc
import Control.Monad
import Control.Monad.Trans.Class
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Semigroup.Applicative
import Data.Tagged
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

-- | Just use map order
todoSorter :: Applicative m => srt -> a -> a -> m Ordering
todoSorter _ _ _ = pure LT

todoFilterer :: TD.Filter -> TD.Todo -> ReadIORef Bool
todoFilterer ftr td = do
    -- scn <- doReadIORef $ modelRef obj
    pure $ case ftr of
        TD.All -> True
        TD.Active -> not $ TD.completed $ td
        TD.Completed -> TD.completed $ td

data App f = App
    { newTodo :: J.JSString
    , todos :: TD.TodoCollection f
    } deriving (G.Generic)

makeLenses_ ''App

type OnNewTodo = Tagged "OnNewTodo"

newTodoInput :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => ReactId -> Widget cmd p J.JSString (OnNewTodo J.JSString)
newTodoInput ri =
    let wid = finish . void . overWindow fw $ W.textInput ri
    in wid `also` lift gad
  where
    fw = (modifyMarkup (overSurfaceProperties
        (<> [("className", "new-todo"), ("placeholder", "What needs to be done?")])))
    gad = (finish hdlMounted)
        `also` (trigger ri "onKeyDown" fireKeyDownKey
            >>= hdlKeyDown)

    hdlMounted ::
        ( AsReactor cmd
        , AsHTMLElement cmd
        )
        => Gadget cmd p J.JSString ()
    hdlMounted = onMounted $ do
        j <- getElementalRef ri
        exec $ Focus j

    hdlKeyDown :: (AsReactor cmd) => KeyDownKey -> Gadget cmd p J.JSString (OnNewTodo J.JSString)
    hdlKeyDown (KeyDownKey _ key) =
        case key of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with W.input onChange
            -- updating the value under our feet.
            "Enter" -> mutateThen $ do
                v <- get
                let v' = J.strip v
                id .= J.empty
                pure $ if J.null v'
                    then finish $ pure ()
                    else pure $ Tagged @"OnNewTodo" v'

            "Escape" -> finish $ mutate $ id .= J.empty

            _ -> finish $ pure ()

appToggleCompleteAll :: (AsReactor cmd)
    => ReactId -> Widget cmd p (TD.TodoCollection Obj) r
appToggleCompleteAll ri =
    let win = do
            scn <- ask
            props <- lift $ mkProps scn
            lf' ri "input" (DL.fromList props)
        gad = -- (finish $ onElementalRef ri) `also`
            (finish $ hdlChange)
    in (display win) `also` (lift gad)
  where
    mkProps :: Model (TD.TodoCollection Obj) -> ReadIORef [JE.Property]
    mkProps scn = traverse sequenceA
        [ ("key", pure . JE.toJSR $ ri)
        , ("className", pure "toggle-all")
        , ("type", pure $ "checkbox")
        , ("checked", JE.toJSR <$> (hasActiveTodos (scn ^. _model.W._visibleList)))
        ]

    hasActiveTodos ::  [Obj TD.Todo] -> ReadIORef Bool
    hasActiveTodos = fmap getAny . getAp . foldMap go
      where
        go obj = Ap $ do
            scn <- doReadIORef $ modelRef obj
            pure $ Any $ scn ^. _model.TD._completed

    hdlChange :: (AsReactor cmd) => Gadget cmd p (TD.TodoCollection Obj) ()
    hdlChange = do
        trigger_ ri "onChange" ()
        put "App ToggleAll"
        mutateThen $ do
            s <- get
            a <- lift $ hasActiveTodos (s ^. W._visibleList)
            pure $ getAls $ foldMap (go a) (view W._visibleList s) -- Only modify visible!
      where
        go a obj = Als $ do
            -- lift from ContT to GadgetT
            lift $ gadgetWith' obj (mutate $ TD._completed .= not a)

app_ :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => JE.JSRep -> Widget cmd p (App Obj) (OnNewTodo J.JSString)
app_ j = do
    todosRi <- mkReactId "todo-list"
    let newTodo' = magnifyWidget _newTodo $ mkReactId "new-todo" >>= newTodoInput
        appToggleCompleteAll' = magnifyWidget _todos $ mkReactId "complete-all" >>= appToggleCompleteAll
        todoFooter' = magnifyWidget _todos $ mkReactId "todo-footer" >>= (TD.todoFooter j)
        todosWindow = modifyMarkup (overSurfaceProperties
            (<> [("className", "todo-list")]))
            (W.dynamicCollectionWindow todosRi)
        wid = withWindow newTodo' $ \newTodoWin ->
            withWindow appToggleCompleteAll' $ \appToggleCompleteAllWin ->
            withWindow todoFooter' $ \todoFooterWin ->
                let win = do
                        s <- ask
                        bh "div" [("key", "app")] $ do
                            bh "header" [("key", "header"), ("className", "header")] $ do
                                bh "h1" [("key", "heading")] (txt "todos")
                                newTodoWin

                            -- only render if there are todos
                            let ts = s ^. _model._todos.W._rawCollection
                            if M.null ts
                                then pure ()
                                else bh "section" [ ("key", "main")
                                                        , ("className", "main")
                                                        ] $ do
                                    -- Complete all checkbox
                                    appToggleCompleteAllWin

                                    -- Render the list of todos
                                    magnifiedModel _todos todosWindow

                                    -- Render the footer
                                    todoFooterWin
                in display win
        gad = magnifiedEntity _todos $ finish $ do
            -- FIXME: this is being called on every keystroke in the input!
            put "App Mutated"
            onMutated $ mutate $ W.updateVisibleList todoFilterer todoSorter
    wid `also` (lift gad)

app :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => JE.JSRep -> Widget cmd p (App Obj) r
app j = app_ j
    >>= (lift . insertTodo')

todoToggleCompleted :: (AsReactor cmd)
    => TD.OnTodoToggleComplete W.UKey -> Gadget cmd p (App Obj) ()
todoToggleCompleted (untag @"OnTodoToggleComplete" -> _) = -- rerender
    mutate (pure ())

destroyTodo :: (AsReactor cmd)
    => TD.OnTodoDestroy W.UKey -> Gadget cmd p (App Obj) ()
destroyTodo (untag @"OnTodoDestroy" -> k) =
    mutate $ _todos.W._rawCollection.(at k) .= Nothing

type OnTodoTicked = Tagged "OnTodoTicked"

tickedTodo :: (AsReactor cmd)
    => OnTodoTicked W.UKey -> Gadget cmd p (App Obj) ()
tickedTodo (untag @"OnTodoTicked" -> _) =
    magnifiedEntity _todos $ mutate $ pure () -- trigger onMutated for App

insertTodo :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => OnNewTodo J.JSString -> Gadget cmd p (App Obj) (Which '[TD.OnTodoToggleComplete W.UKey, TD.OnTodoDestroy W.UKey, OnTodoTicked W.UKey])
insertTodo (untag @"OnNewTodo" -> n) = do
    s <- getModel
    let mk = view (_todos.W._rawCollection.to lookupMax) s
        k' = case mk of
            Just (k, _) -> W.largerUKey k
            Nothing -> W.zeroUKey
    withMkObj (go k' <$> todo') (TD.Todo n False False) $ \obj ->
        mutate $ _todos.W._rawCollection.(at k') .= Just obj
  where
    lookupMax = listToMaybe . M.toDescList
    todo' = TD.todo
        & chooseWith also $ (onMutated (pure $ pickOnly $ Tagged @"OnTodoTicked" ()))
    go :: W.UKey -> Which '[TD.OnTodoToggleComplete (), TD.OnTodoDestroy (), OnTodoTicked ()] -> Which '[TD.OnTodoToggleComplete W.UKey, TD.OnTodoDestroy W.UKey, OnTodoTicked W.UKey]
    go k y = afmap (CaseFunc1 @C0 @Functor @C0 (fmap (const k))) y

insertTodo' :: (AsReactor cmd, AsJavascript cmd, AsHTMLElement cmd)
    => OnNewTodo J.JSString -> Gadget cmd p (App Obj) r
insertTodo' a = insertTodo a
    >>= (injectedK $ totally . finish . todoToggleCompleted . obvious)
    >>= (injectedK $ totally . finish . destroyTodo . obvious)
    >>= (injectedK $ totally . finish . tickedTodo . obvious)
    & fmap impossible
