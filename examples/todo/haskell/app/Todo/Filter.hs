module Todo.Filter where

data Filter = All | Active | Completed
    deriving (Eq, Show)
