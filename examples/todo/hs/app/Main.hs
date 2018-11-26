{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Lens
import Control.Monad.IO.Unlift
import Data.Diverse.Profunctor
import Data.Foldable
import Data.Proxy
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

-- | Add a newtype wrapper to allow recursive definition
newtype AppCmd = AppCmd { unAppCmd :: Which (CmdTypes AppCmd AppCmd)}
    -- deriving Show
-- The main app will need IO effects in order to get the initial state via MVar.
type instance CmdTypes AppCmd cmd = '[IO cmd, [cmd], ReactorCmd cmd, HTMLElementCmd, JavaScriptCmd cmd]

-- | Define AsFacet instances for all types in the variant
-- UndecidableInstances!
instance (AsFacet a (Which (CmdTypes AppCmd AppCmd))) => AsFacet a AppCmd where
    facet = iso unAppCmd AppCmd . facet

maybeExecApp ::
    ( MonadUnliftIO m
    , MonadReader r m
    , Has ReactorEnv r
    , AsFacet (IO cmd) cmd
    , AsReactor cmd
    , AsJavascript cmd
    , AsHTMLElement cmd
    )
    => (cmd -> m ()) -> cmd -> MaybeT m (Proxy '[[cmd], ReactorCmd cmd, JavaScriptCmd cmd, HTMLElementCmd, IO cmd], [cmd])
maybeExecApp executor c =
    maybeExec (done (traverse_ @[] executor)) c
    `orMaybeExec` maybeExec (execReactorCmd executor) c
    `orMaybeExec` maybeExec (done (execJavascript executor)) c
    `orMaybeExec` maybeExec (done execHTMLElementCmd) c
    `orMaybeExec` maybeExec (done ((>>= executor) . liftIO)) c
  where
    done f b = (\() -> []) <$> (f b)

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
-- GHCJS runs 'main' lazily.
-- The code here only start running after all javascript is loaded.
-- Ie. h$main(h$mainZCZCMainzimain) just schedules the work to be executed after all javascript is loaded.
main :: IO ()
main = do
    root <- js_getElementById "root"
    rEnv <- mkReactorEnvIO
    (`runReaderT` rEnv) $ startApp
        execApp
        wid
        (TD.App "" (W.DynamicCollection TD.All () mempty mempty))
        (JE.toJSR root)
  where
    execApp = verifyAndFixExec' unAppCmd maybeExecApp
    wid = noIOWidget (TD.app documentDefaultView) (TD.app documentDefaultView)

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
