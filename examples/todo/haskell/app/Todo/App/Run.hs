{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.App.Run where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import qualified Glazier.React.Command.Run as R
import qualified Glazier.React.Component as R
import qualified Glazier.React.Widgets.Input.Run as W.Input
import qualified Glazier.React.Widgets.List as W.List
import qualified Glazier.React.Widgets.List.Run as W.List
import qualified Pipes.Concurrent as PC
import Todo.App as TD.App
import qualified Todo.Footer.Run as TD.Footer
import qualified Todo.Todo.Run as TD.Todo

run :: R.ReactComponent -> PC.Output Action -> Command -> IO ()

run _ _ (RenderCommand sm props j) = R.componentSetState sm props j

run _ output (SendActionsCommand acts) =
    void $ runMaybeT $
    traverse_ (\act -> lift $ atomically $ PC.send output act >>= guard) acts

run _ _ (InputCommand cmd) = W.Input.run cmd

run comp output (TodosCommand cmd) =
    W.List.run
        (\k -> TD.Todo.run (contramap (TodosAction . W.List.ItemAction k) output))
        comp
        (contramap TodosAction output)
        cmd

run _ _ (FooterCommand cmd) = TD.Footer.run cmd
