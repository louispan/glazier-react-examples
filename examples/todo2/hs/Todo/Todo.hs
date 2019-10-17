{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Todo.Todo where
    -- ( Todo(..)
    -- , _value
    -- , _completed
    -- , _editing
    -- , TodoDestroy
    -- , TodoToggleComplete
    -- , todo
    -- ) where

-- import qualified Data.Aeson as A
-- import qualified Data.Aeson.Applicative as A

-- import qualified Data.DList as DL
import qualified Data.JSString as J
import qualified GHC.Generics as G
import qualified Glazier.DOM as DOM
import Glazier.React
import Glazier.React.Widgets.Input

data Todo = Todo
    { value :: JSString
    , completed :: Bool
    , editing :: Bool
    } deriving (Show, Eq, Ord, G.Generic)

makeLenses_ ''Todo

-- instance A.ToJSON Todo where toEncoding = A.genericToEncoding A.defaultOptions
-- instance Applicative m => A.AToJSON m Todo
-- instance A.FromJSON Todo
-- instance Applicative m => A.AFromJSON m Todo

-- type OnTodoDestroy = Tagged "TodoDestroy"
-- type OnTodoToggleComplete = Tagged "TodoToggleComplete"
-- type OnTodoStartEdit = Tagged "TodoStartEdit"
-- data TodoDestroy = TodoDestroy
-- data TodoToggleComplete = TodoToggleComplete
-- data TodoStartEdit = TodoStartEdit

todoToggleComplete ::
    (MonadWidget s c m, MonadObserver (Tagged "TodoToggleComplete" ()) m)
    => Traversal' s Todo -> m ()
todoToggleComplete this = checkbox (Tagged @"TodoToggleComplete" ()) (this._completed)
    [] [("className", strProp "toggle")]

todoDestroy :: (MonadWidget' c m, MonadObserver (Tagged "TodoDestroy" ()) m) => m ()
todoDestroy = lf' (jsstr "button") [("onClick", onClick)]
    [("key", strProp' "destroy"),("className", strProp' "destroy")]
  where
    onClick = mkSyntheticHandler (const $ pure ()) $
        const $ observe $ Tagged @"TodoDestroy" ()

todoLabel :: (MonadWidget s c m, MonadObserver (Tagged "TodoStartEdit" ()) m)
    => Traversal' s Todo -> m ()
todoLabel this = bh' (jsstr "label") [("onDoubleClick", onDoubleClick)] []
    (strTxt (view $ this._value))
  where
    onDoubleClick = mkSyntheticHandler (const $ pure ()) $
        const $ observe $ Tagged @"TodoStartEdit" ()

todoInput ::
    ( MonadWidget s c m
    , MonadObserver (Tagged "InputChange" ()) m
    , MonadObserver (Tagged "TodoDestroy" ()) m
    )
    => Traversal' s Todo
    -> m ()
todoInput this = input (Tagged @"InputChange" ()) (this._value)
    [("onBlur", onBlur), ("onKeyDown", onKeyDown)]
    [("className", strProp "edit")]
  where
    onBlur = mkSyntheticHandler (const $ pure ()) (const $
        mutate' $ do
            this._editing .= False
            this._value %= J.strip)

    onKeyDown = mkSyntheticHandler fromKeyDown hdlKeyDown
    fromKeyDown evt = maybeM $ pure $ (\t' e' -> (t', DOM.key e')) <$> t <*> e
      where
        e = fromJS' @DOM.SyntheticKeyboardEvent evt
        t = fromJS' @DOM.HTMLElement $ DOM.target evt
    hdlKeyDown (t, k) = case k of
            -- NB. Enter and Escape doesn't generate a onChange event
            -- So there is no adverse interation with input onChange
            -- updating the value under our feet.
            "Enter" -> mutateThen' $ do
                v <- use (this._value)
                let v' = J.strip v
                if J.null v'
                    then
                        pure $ observe $ Tagged @"TodoDestroy" ()
                    else do
                        this._editing .= False
                        this._value .= v'
                        pure $ pure ()

            "Escape" -> DOM.blur t -- The onBlur handler will trim the value

            _ -> pure ()


-- todoView ::
--     ( HasCallStack
--     , AsReactor c
--     , MonadWidget c s m
--     , Observer (Tagged "TodoToggleComplete" ()) m
--     , Observer (Tagged "TodoDestroy" ()) m
--     , Observer (Tagged "TodoStartEdit" ()) m
--     )
--     => Traversal' s Todo
--     -> Widget m ()
-- todoView this = bh "div" [] [("className", strProp "view")] $
--     todoToggleComplete this
--     <> todoLabel this
--     <> todoDestroy

-- todo ::
--     ( HasCallStack
--     , AsReactor c
--     , AsJavascript c
--     , AsHTMLElement c
--     , MonadWidget c s m
--     , Observer (Tagged "TodoToggleComplete" ()) m
--     , Observer (Tagged "TodoDestroy" ()) m
--     , Observer (Tagged "InputChange" ()) m
--     )
--     => Traversal' s Todo
--     -> Widget m ()
-- todo this = bh "li" [] [("className", classNames
--                         [("completed", maybeM $ preview (this._completed))
--                         ,("editing", maybeM $ preview (this._editing))])]
--                 (todoView' <> todoInput this)
--   where
--     todoView' = (`runObserver` hdlStartEdit) `fmap2` todoView this
--     -- hdlStartEdit :: Tagged "TodoStartEdit" () -> m ()
--     hdlStartEdit (untag @"TodoStartEdit" -> ()) = do
--         mutate $ this._editing .= True
--         j <- getReactRef
--         onRenderedOnce $ do
--             focus j
--             v <- fromProperty "value" j
--             let i = J.length v
--             setProperty ("selectionStart", (0 :: Int)) j
--             setProperty ("selectionEnd", i) j


