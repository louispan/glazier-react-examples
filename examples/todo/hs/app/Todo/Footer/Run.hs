module Todo.Footer.Run
    ( run
    ) where

import qualified Glazier.React.Command.Run as R
import Todo.Footer

run :: Command -> IO ()

run (RenderCommand sm props j) = R.componentSetState sm props j
