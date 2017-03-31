{- subsample
Gregory W. Schwartz

Subsample data from a uniform distribution so two different statuses for the same entity have the same number of samples.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Standard
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Cabal
import Data.Csv
import Options.Generic
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Vector as V
       
-- Local
import Types
import Load
import Subsample

-- | Command line arguments
data Options = Options { sampleCol  :: Maybe T.Text
                               <?> "([name] | COLUMN) The column containing the names of the samples."
                       , statusCol :: Maybe T.Text
                               <?> "([status] | COLUMN) The column containing the statuses of the entities."
                       , n :: Maybe Int
                               <?> "([MINIMUM] | INT) The number of samples to pull from each status. The default value is the sample number of the status with the smallest number of samples."
                       }
               deriving (Generic)

instance ParseRecord Options

main :: IO ()
main = do
    opts <- getRecord "subsample, Gregory W. Schwartz.\
                      \ Subsample data from a uniform distribution so two\
                      \ different statuses for all entities have the same\
                      \ number of samples."

    contents <- BL.getContents

    let sampleCol'           =
            SampleCol . fromMaybe "sample" . unHelpful . sampleCol $ opts
        statusCol'           =
            StatusCol . fromMaybe "status" . unHelpful . statusCol $ opts
        (outputHeader, rows) = either error id
             $ (decodeByName contents :: Either String (Header, V.Vector (Map.Map T.Text T.Text)))
        statusMap            = getStatusMap statusCol' sampleCol' rows
        smallest             =
            maybe (smallestSampleNum statusMap) Size . unHelpful . n $ opts
        
    validSamples <- getSubsampling smallest statusMap
        
    let outputBody = filterRowsValid
                        (unSampleCol sampleCol')
                        (Set.map unSample validSamples)
                   . V.toList
                   $ rows

    BL.putStrLn . encodeByName outputHeader $ outputBody

    return ()
