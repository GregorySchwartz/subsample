{- Load
Gregory W. Schwartz

Collects the functions pertaining to loading the input data.
-}

{-# LANGUAGE BangPatterns #-}

module Load
    ( getStatusMap
    ) where

-- Standard
import Data.Semigroup
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

-- Cabal
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local
import Types
import Utility

-- | Find the samples within each status.
getStatusMap :: StatusCol
             -> SampleCol
             -> V.Vector (Map.Map T.Text T.Text)
             -> StatusMap
getStatusMap (StatusCol statusCol) (SampleCol sampleCol) =
    StatusMap
        . Map.map (Seq.fromList . Set.toList . Set.fromList . F.toList)
        . Map.fromListWith (Seq.><)
        . fmap (\ !m -> ( Status . lookupWithErr statusCol $ m
                        , Seq.singleton . Sample . lookupWithErr sampleCol $ m
                        )
               )
        . V.toList
