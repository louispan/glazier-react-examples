{-# LANGUAGE CPP #-}
module Todo.Todo.Run
    ( run
    ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified GHCJS.Types as J
import qualified Glazier.React.Command.Run as R
import qualified JavaScript.Extras as JE
import qualified Pipes.Concurrent as PC
import Todo.Todo

run :: PC.Output Action -> Command -> IO ()

run _ (SetPropertyCommand prop j) = JE.setProperty prop j

run _ (RenderCommand sm props j) = R.componentSetState sm props j

run _ (FocusNodeCommand j) = js_focus j

run output (SendActionsCommand acts) =
    void $ runMaybeT $
    traverse_ (\act -> lift $ atomically $ PC.send output act >>= guard) acts

#ifdef __GHCJS__

foreign import javascript unsafe
  "if ($1 && $1['focus']) { $1['focus'](); }"
  js_focus :: J.JSVal -> IO ()

#else

js_focus :: J.JSVal -> IO ()
js_focus _ = pure ()

#endif
