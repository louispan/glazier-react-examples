{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Extras
import qualified Glazier.DOM as DOM
import Glazier.React.Main
import Todo.App
import Todo.Todos

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
-- GHCJS runs 'main' lazily.
-- The code here only starts running after all javascript is loaded.
-- Ie. h$main(h$mainZCZCMainzimain) just schedules the work to be executed after all javascript is loaded.
main :: IO ()
main = (`evalMaybeT` ()) $ do
    d <- guardJust $ DOM.globalDocument
    root <- guardJustM $ DOM.getElementById d "root"

    liftIO $ void $ simpleWidgetMain
        (LogConfig (Just DEBUG) Nothing mempty)
        (1000000 `div` 60)
        root
        (app id)
        "app"
        (App "" (Todos All mempty))
