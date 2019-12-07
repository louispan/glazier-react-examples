{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Trans.Extras
import Glazier.React.Main
import qualified JS.DOM as DOM
import Todo.App
import Todo.Todos

default (JSString)

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
-- GHCJS runs 'main' lazily.
-- The code here only starts running after all javascript is loaded.
-- Ie. h$main(h$mainZCZCMainzimain) just schedules the work to be executed after all javascript is loaded.
main :: IO ()
main = (`evalMaybeT` ()) $ do
    d <- guardJustIO $ fromJS @DOM.Document <$> globalThis `getProperty` "document"
    root <- guardJustM $ DOM.getElementById d "root"

    liftIO $ void $ simpleWidgetMain
        (LogConfig (Just DEBUG) Nothing mempty)
        (1000000 `div` 60)
        root
        (app id)
        -- (app2)
        "app"
        (App "" (Todos All mempty))
