{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont
import Data.Diverse.Profunctor
import qualified Data.DList as DL
import Data.Foldable
import Data.Proxy
import Data.Tagged
import qualified GHCJS.Foreign.Export as J
import qualified GHCJS.Types as J
import Glazier.React
import Glazier.React.Effect.HTMLElement
import Glazier.React.Effect.HTMLElement.Exec
import Glazier.React.Effect.JavaScript
import Glazier.React.Effect.JavaScript.Exec
import qualified Glazier.React.Widgets.Collection.Dynamic as W
import qualified JavaScript.Extras as JE
import qualified Todo.App as TD
import qualified Todo.Filter as TD

-- Base effects do not contain IO.
-- This is used to ensure that Widgets do not contain IO effects.
-- See 'verifiedApp'
type BaseAppEffects cmd = '[[cmd], ReactorCmd cmd, HTMLElementCmd, JavaScriptCmd cmd]

-- However the main app will need IO effects in order to get the initial state via MVar.
type AppEffects cmd = IO cmd ': BaseAppEffects cmd

-- | Add a newtype wrapper to allow recursive definition
newtype AppCmd = AppCmd { unAppCmd :: Which (AppEffects AppCmd)}
    -- deriving Show

newtype BaseAppCmd = BaseAppCmd { unBaseAppCmd :: Which (BaseAppEffects BaseAppCmd)}
    deriving Show

-- | Define AsFacet instances for all types in the variant
-- UndecidableInstances!
instance (AsFacet a (Which (AppEffects AppCmd))) => AsFacet a AppCmd where
    facet = iso unAppCmd AppCmd . facet
instance (AsFacet a (Which (BaseAppEffects BaseAppCmd))) => AsFacet a BaseAppCmd where
    facet = iso unBaseAppCmd BaseAppCmd . facet

-- | unused-top-binds. Howevver, it is used to verify at compile time
-- that BaseAppCmd fulfill all the AsFacet requirements.
verifiedApp :: JE.JSRep -> Widget BaseAppCmd p (TD.App Subject) r
verifiedApp j = TD.app j

maybeExecApp ::
    ( AsFacet (IO cmd) cmd
    , AsReactor cmd
    , AsJavascript cmd
    , AsHTMLElement cmd
    , MonadUnliftIO m
    , Has (Tagged ReactId (MVar Int)) r
    , MonadReader r m
    )
    => (cmd -> m ()) -> cmd -> MaybeT m (Proxy '[[cmd], ReactorCmd cmd, JavaScriptCmd cmd, HTMLElementCmd, IO cmd] , ())
maybeExecApp exec c =
    maybeExec (traverse_ @[] exec) c
    `orMaybeExec` maybeExec (execReactorCmd exec) c
    `orMaybeExec` maybeExec (execJavascript exec) c
    `orMaybeExec` maybeExec execHTMLElementCmd c
    `orMaybeExec` maybeExec ((>>= exec) . liftIO) c

execApp ::
    ( MonadUnliftIO m
    , Has (Tagged ReactId (MVar Int)) r
    , MonadReader r m
    ) => AppCmd -> m ()
execApp = verifyExec unAppCmd (fixExec maybeExecApp)

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
-- GHCJS runs 'main' lazily.
-- The code here only start running after all javascript is loaded.
-- Ie. h$main(h$mainZCZCMainzimain) just schedules the work to be executed after all javascript is loaded.
main :: IO ()
main = do
    riVar <- newMVar (0 :: Int)
    sbjVar <- newEmptyMVar
    let setup :: ContT () (State (DL.DList AppCmd)) ()
        setup = do
            sbj <- mkSubject' (TD.app documentDefaultView) (TD.App ""
                (W.DynamicCollection TD.All () mempty mempty))
            postCmd' (command_ <$> (putMVar sbjVar sbj))
        cs = (`execState` mempty) $ evalContT setup
    -- run the initial commands, this will store the app Subject into sbjVar
    (`runReaderT` (Tagged @ReactId riVar)) $ traverse_ execApp cs

    -- Start the App render
    sbj <- takeMVar sbjVar
    root <- js_getElementById "root"
    markup <- unReadIORef $ (`execStateT` mempty) $ displaySubject sbj
    e <- toElement markup
    renderDOM e (JE.toJSR root)

    -- Export sbj to prevent it from being garbage collected
    void $ J.export sbj

documentDefaultView :: JE.JSRep
documentDefaultView = JE.toJSR js_documentDefaultView

#ifdef __GHCJS__

foreign import javascript unsafe
  "document.defaultView"
  js_documentDefaultView :: J.JSVal

foreign import javascript unsafe
  "$r = document.getElementById($1);"
  js_getElementById :: J.JSString -> IO J.JSVal

#else

js_documentDefaultView :: J.JSVal
js_documentDefaultView = J.nullRef

js_getElementById :: J.JSString -> IO J.JSVal
js_getElementById _ = pure J.nullRef

#endif
