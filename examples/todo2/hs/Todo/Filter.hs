{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Todo.Filter where

-- import qualified Data.Aeson as A
-- import qualified Data.Aeson.Applicative as A
import qualified GHC.Generics as G
-- import qualified Glazier.React.Widgets.Collection.Dynamic as W
-- import qualified Todo.Todo as TD

data Filter = All | Active | Completed
    deriving (Eq, Show, Ord, G.Generic)

-- instance A.ToJSON Filter where toEncoding = A.genericToEncoding A.defaultOptions
-- instance Applicative m => A.AToJSON m Filter
-- instance A.FromJSON Filter
-- instance Applicative m => A.AFromJSON m Filter

-- instance W.FilterPredicate Filter TD.Todo where
--     filterPredicate ftr td = do
--         pure $ case ftr of
--             All -> True
--             Active -> not $ TD.completed $ td
--             Completed -> TD.completed $ td
