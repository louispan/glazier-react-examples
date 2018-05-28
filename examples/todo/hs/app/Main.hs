-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeApplications #-}

module Main (main) where

-- import Control.Concurrent.MVar
-- import Control.Concurrent.STM
-- import qualified Control.Disposable as CD
-- import Control.Lens
-- import Control.Monad.Free.Church
-- import Control.Monad.IO.Class
-- import Control.Monad.Morph
-- import Control.Monad.State.Strict
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.Reader
-- import qualified Data.DList as D
-- import Data.Foldable
-- import qualified Data.JSString as J
-- import Data.Maybe
-- import qualified GHCJS.Foreign.Callback as J
-- import qualified GHCJS.Marshal.Pure as J
-- import qualified GHCJS.Types as J
-- import qualified Glazier as G
-- import Glazier.React.Component
-- import Glazier.React.Maker
-- import Glazier.React.Maker.Run.Maker
-- import Glazier.React.Markup
-- import Glazier.React.Model
-- import Glazier.React.ReactDOM as RD
-- import Glazier.React.Widget
-- import Glazier.React.Widgets.Input as W.Input
-- import Glazier.React.Widgets.List as W.List
-- import qualified Pipes as P
-- import qualified Pipes.Concurrent as PC
-- import qualified Pipes.Lift as PL
-- import qualified Pipes.Misc as PM
-- import qualified Pipes.Prelude as PP
import qualified Todo.App as TD.App
-- import qualified Todo.App.Run as TD.App
import qualified Todo.Filter as TD.Filter
import qualified Todo.Footer as TD.Footer

main :: IO ()
main = pure ()

-- appOutline :: TD.App.Outline
-- appOutline = TD.App.Schema
--     (W.Input.Schema
--         "What needs to be done?"
--         "new-todo")
--     (W.List.Schema
--         "todo-list"
--          0
--          mempty
--          (const True))
--     (TD.Footer.Schema 0 0 TD.Filter.All)

-- -- | 'main' is used to create React classes and setup callbacks to be used externally by the browser.
-- -- GHCJS runs 'main' lazily.
-- -- The code here only start running after all javascript is loaded.
-- -- Ie. h$main(h$mainZCZCMainzimain) just schedules the work to be executed after all javascript is loaded.
-- main :: IO ()
-- main = do
--     -- Create a 'Pipes.Concurrent' mailbox for receiving actions from html events.
--     -- NB. using 'PC.bounded 1' also works without deadlocks, but doesn't save memory
--     -- blocked events are kept by the GHCJCS runtime.
--     (output, input) <- PC.spawn $ PC.unbounded

--     muid <- newMVar 0

--     component <- mkComponent

--     let appWidget = TD.App.widget mempty
--     -- App Model
--     s <- iterM (Maker.run muid component output) (mkElement' appWidget appOutline)

--     -- Start the App render
--     root <- js_getElementById "root"
--     e <- markedElement (hoist (hoist generalize) (window appWidget)) (s ^. scene)
--     RD.render (J.pToJSVal e) root

--     -- The footer uses hashChange to fire events
--     onHashChange' <- iterM (Maker.run muid component output)
--         (hoistWithAction TD.App.FooterAction (mkHandler TD.Footer.onHashChange))
--     js_addHashChangeListener onHashChange'

--     -- Fire the initial hashChange action
--     hash <- js_getHash
--     void $ runMaybeT $ do
--         acts <- fmap TD.App.FooterAction <$> TD.Footer.withHashChange hash
--         traverse_ (\ini -> lift $ atomically $ PC.send output ini >>= guard) acts

--     -- Run the gadget effect which reads actions from 'Pipes.Concurrent.Input'
--     -- and notifies html React of any state changes.
--     -- runEffect will only stop if input is finished (which in this example never does).
--     s' <- P.runEffect $ appEffect (gadget appWidget) s muid component output input

--     -- Cleanup
--     -- We actually never get here because in this example runEffect never quits
--     -- but in other apps, gadgetEffect might be quit-able (eg with MaybeT)
--     -- so let's just be explicit where cleanup code would be.
--     CD.dispose (CD.disposing s')
--     CD.dispose (CD.disposing onHashChange')

-- appEffect
--     :: MonadIO io
--     => GadgetOf TD.App.Widget
--     -> ElementOf TD.App.Widget
--     -> MVar Int
--     -> ReactComponent
--     -> PC.Output TD.App.Action
--     -> PC.Input TD.App.Action
--     -> P.Effect io (ElementOf TD.App.Widget)
-- appEffect appGadget s muid component output input =
--     PL.execStateP s $
--         appProducerIO appGadget input P.>->
--         runCommandsPipe muid component output P.>->
--         PP.drain

-- appProducerIO
--     :: MonadIO io
--     => GadgetOf TD.App.Widget
--     -> PC.Input TD.App.Action
--     -> P.Producer' (D.DList TD.App.Command) (StateT (ElementOf TD.App.Widget) io) ()
-- appProducerIO appGadget input = hoist (hoist (liftIO . atomically)) (appProducer appGadget input)

-- appProducer
--     :: GadgetOf TD.App.Widget
--     -> PC.Input TD.App.Action
--     -> P.Producer' (D.DList TD.App.Command) (StateT (ElementOf TD.App.Widget) STM) ()
-- appProducer appGadget input = PM.execInput input go'
--   where
--     go = (runMaybeT .) . runReaderT . G.runGadgetT $ hoist generalize appGadget
--     go' = fmap (fromMaybe mempty) <$> go

-- runCommandsPipe
--     :: (MonadState (ElementOf TD.App.Widget) io, MonadIO io)
--     => MVar Int
--     -> ReactComponent
--     -> PC.Output TD.App.Action
--     -> P.Pipe (D.DList TD.App.Command) () io ()
-- runCommandsPipe muid component output = PP.mapM (runCommands muid component output)

-- runCommands
--     :: (Foldable t, MonadState (ElementOf TD.App.Widget) io, MonadIO io)
--     => MVar Int
--     -> ReactComponent
--     -> PC.Output TD.App.Action
--     -> t TD.App.Command
--     -> io ()
-- runCommands muid component output = traverse_ (liftIO . TD.App.run muid component output)

-- #ifdef __GHCJS__

-- foreign import javascript unsafe
--   "$r = document.getElementById($1);"
--   js_getElementById :: J.JSString -> IO J.JSVal

-- foreign import javascript unsafe
--   "window.addEventListener('hashchange', $1, false);"
--   js_addHashChangeListener :: J.Callback a -> IO ()

-- foreign import javascript unsafe
--   "$r = window.location.hash;"
--   js_getHash :: IO J.JSString

-- #else

-- js_getElementById :: J.JSString -> IO J.JSVal
-- js_getElementById _ = pure J.nullRef

-- js_addHashChangeListener :: J.Callback a -> IO ()
-- js_addHashChangeListener _ = pure ()

-- js_getHash :: IO J.JSString
-- js_getHash = pure J.empty


-- #endif
