{- Utility
Gregory W. Schwartz

Collects the helper functions of the program.
-}

module Utility
    ( lookupWithErr
    ) where

-- Standard
import Data.Semigroup
import qualified Data.Map.Strict as Map

-- Cabal

-- Local

-- | Better error message for lookup.
lookupWithErr :: (Ord k, Show k) => k -> Map.Map k v -> v
lookupWithErr k =
    Map.findWithDefault (error $ "Cannot find column: " <> show k) k
