{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Disposable as CD
import Control.Lens
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.DList as D
import Data.Foldable
import qualified Data.JSString as J
import qualified GHCJS.Foreign.Callback as J
import qualified GHCJS.Marshal.Pure as J
import qualified GHCJS.Types as J
import qualified Glazier as G
import qualified Glazier.React.Component as R
import qualified Glazier.React.Maker as R
import qualified Glazier.React.Maker.Run as R.Maker
import qualified Glazier.React.Markup as R
import qualified Glazier.React.Model as R
import qualified Glazier.React.ReactDOM as RD
import qualified Glazier.React.Widget as R
import qualified Glazier.React.Widgets.Input as W.Input
import qualified Glazier.React.Widgets.List as W.List
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Misc as PM
import qualified Pipes.Prelude as PP
import qualified Todo.App as TD.App
import qualified Todo.App.Run as TD.App
import qualified Todo.Footer as TD.Footer
import qualified Todo.Filter as TD.Filter

appOutline :: TD.App.Outline
appOutline = TD.App.Schema
    (W.Input.Schema
        "What needs to be done?"
        "new-todo")
    (W.List.Schema
        "todo-list"
         0
         mempty
         (const True))
    (TD.Footer.Schema 0 0 TD.Filter.All)

-- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
-- GHCJS runs 'main' lazily.
-- The code here only start running after all javascript is loaded.
-- Ie. h$main(h$mainZCZCMainzimain) just schedules the work to be executed after all javascript is loaded.
main :: IO ()
main = do
    -- Create a 'Pipes.Concurrent' mailbox for receiving actions from html events.
    -- NB. using 'PC.bounded 1' also works without deadlocks, but doesn't save memory
    -- blocked events are kept by the GHCJCS runtime.
    (output, input) <- PC.spawn $ PC.unbounded

    muid <- newMVar 0

    component <- R.mkComponent

    let appWidget = TD.App.widget mempty
    -- App Model
    s <- iterM (R.Maker.run muid component output) (R.mkGizmo' appWidget appOutline)

    -- Start the App render
    root <- js_getElementById "root"
    e <- R.markedElement (hoist (hoist generalize) (R.window appWidget)) (s ^. R.scene)
    RD.render (J.pToJSVal e) root

    -- The footer uses hashChange to fire events
    onHashChange' <- iterM (R.Maker.run muid component output)
        (R.hoistWithAction TD.App.FooterAction (R.mkHandler TD.Footer.onHashChange))
    js_addHashChangeListener onHashChange'

    -- Fire the initial hashChange action
    hash <- js_getHash
    void $ runMaybeT $ do
        acts <- fmap TD.App.FooterAction <$> TD.Footer.withHashChange hash
        traverse_ (\act -> lift $ atomically $ PC.send output act >>= guard) acts

    -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
    -- and notifies html React of any state changes.
    -- runEffect will only stop if input is finished (which in this example never does).
    s' <- P.runEffect $ appEffect (R.gadget appWidget) s muid component output input

    -- Cleanup
    -- We actually never get here because in this example runEffect never quits
    -- but in other apps, gadgetEffect might be quit-able (eg with MaybeT)
    -- so let's just be explicit where cleanup code would be.
    CD.dispose (CD.disposing s')
    CD.dispose (CD.disposing onHashChange')

appEffect
    :: MonadIO io
    => R.GadgetOf TD.App.Widget
    -> R.GizmoOf TD.App.Widget
    -> MVar Int
    -> R.ReactComponent
    -> PC.Output TD.App.Action
    -> PC.Input TD.App.Action
    -> P.Effect io (R.GizmoOf TD.App.Widget)
appEffect appGadget s muid component output input =
    PL.execStateP s $
        appProducerIO appGadget input P.>->
        runCommandsPipe muid component output P.>->
        PP.drain

appProducerIO
    :: MonadIO io
    => R.GadgetOf TD.App.Widget
    -> PC.Input TD.App.Action
    -> P.Producer' (D.DList TD.App.Command) (StateT (R.GizmoOf TD.App.Widget) io) ()
appProducerIO appGadget input = hoist (hoist (liftIO . atomically)) (appProducer appGadget input)

appProducer
    :: R.GadgetOf TD.App.Widget
    -> PC.Input TD.App.Action
    -> P.Producer' (D.DList TD.App.Command) (StateT (R.GizmoOf TD.App.Widget) STM) ()
appProducer appGadget input = PM.execInput input go'
  where
    go = (runExceptT .) . runReaderT . G.runGadgetT $ hoist generalize appGadget
    go' = fmap (either (const mempty) id) <$> go

runCommandsPipe
    :: (MonadState (R.GizmoOf TD.App.Widget) io, MonadIO io)
    => MVar Int
    -> R.ReactComponent
    -> PC.Output TD.App.Action
    -> P.Pipe (D.DList TD.App.Command) () io ()
runCommandsPipe muid component output = PP.mapM (runCommands muid component output)

runCommands
    :: (Foldable t, MonadState (R.GizmoOf TD.App.Widget) io, MonadIO io)
    => MVar Int
    -> R.ReactComponent
    -> PC.Output TD.App.Action
    -> t TD.App.Command
    -> io ()
runCommands muid component output = traverse_ (liftIO . TD.App.run muid component output)

#ifdef __GHCJS__

foreign import javascript unsafe
  "$r = document.getElementById($1);"
  js_getElementById :: J.JSString -> IO J.JSVal

foreign import javascript unsafe
  "window.addEventListener('hashchange', $1, false);"
  js_addHashChangeListener :: J.Callback a -> IO ()

foreign import javascript unsafe
  "$r = window.location.hash;"
  js_getHash :: IO J.JSString

#else

js_getElementById :: J.JSString -> IO J.JSVal
js_getElementById _ = pure J.nullRef

js_addHashChangeListener :: J.Callback a -> IO ()
js_addHashChangeListener _ = pure ()

js_getHash :: IO J.JSString
js_getHash = pure J.empty


#endif
