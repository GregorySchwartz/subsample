{- Subsample
Gregory W. Schwartz

Collects the functions pertaining to subsampling the statuses with more samples.
-}

{-# LANGUAGE BangPatterns #-}

module Subsample
    ( smallestSampleNum
    , getSubsampling
    , filterRowsValid
    ) where

-- Standard
import Data.Semigroup
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Traversable as T

-- Cabal
import Data.Random
import qualified Data.Vector as V

-- Local
import Types
import Utility

-- | Find the smallest number of samples in a StatusMap.
smallestSampleNum :: StatusMap -> Size
smallestSampleNum = Size . F.minimum . Map.map Seq.length . unStatusMap

-- | Get the valid samples of a StatusMap by subsampling a certain number.
getSubsampling :: Size -> StatusMap -> IO (Set.Set Sample)
getSubsampling (Size size) =
    fmap (Set.fromList . concat . Map.elems)
        . T.mapM (\ xs -> sample
                        . shuffleNofM size (Seq.length xs)
                        . F.toList
                        $ xs
                 )
        . unStatusMap

-- | Filter rows based on whether they contain valid values in a column.
filterRowsValid
    :: (Eq a, Eq b, Ord a, Ord b, Show a)
    => a -> Set.Set b -> [Map.Map a b] -> [Map.Map a b]
filterRowsValid col valid = filter (flip Set.member valid . lookupWithErr col)
