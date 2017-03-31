{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

module Types where

-- Standard
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- Cabal
import qualified Data.Text as T

-- Local


-- Basic
newtype SampleCol  = SampleCol { unSampleCol :: T.Text }
newtype StatusCol  = StatusCol T.Text
newtype Sample     = Sample { unSample :: T.Text } deriving (Eq, Ord)
newtype Status     = Status T.Text deriving (Eq, Ord)
newtype Size       = Size Int

-- Advanced
newtype StatusMap = StatusMap { unStatusMap :: Map.Map Status (Seq.Seq Sample) }

newtype OutputMap = OutputMap { unOutputMap :: Map.Map T.Text T.Text }
