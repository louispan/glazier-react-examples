{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.App.Run where

import Control.Concurrent.MVar
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

run :: MVar Int -> R.ReactComponent -> PC.Output Action -> Command -> IO ()

run _ _ _ (RenderCommand sm props j) = R.componentSetState sm props j

run _ _ output (SendTodosActionsCommand acts) =
    void $ runMaybeT $
    traverse_ (\ini -> lift $ atomically $ PC.send output ini >>= guard) (TodosAction <$> acts)

run _ _ output (SendFooterActionCommand ini) =
    void $ atomically $ PC.send output (FooterAction ini)

run _ _ _ (InputCommand cmd) = W.Input.run cmd

run muid comp output (TodosCommand cmd) =
    W.List.run
        (\k -> TD.Todo.run (contramap (TodosAction . W.List.ItemAction k) output))
        muid
        comp
        (contramap TodosAction output)
        cmd

run _ _ _ (FooterCommand cmd) = TD.Footer.run cmd
