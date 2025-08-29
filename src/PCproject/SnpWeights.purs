module PCproject.SnpWeights where

import Prelude

import Control.Monad.ST as ST
import Data.Array ((..), (!!), head, drop, replicate, length, zip)
import Data.Array.ST as STA
import Data.ArrayBuffer.Typed (empty, set, Index)
import Data.ArrayBuffer.Types (Float32Array, Uint32Array)
import Data.Either (Either(..))
import Data.Float32 (Float32)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Number as DN
import Data.String.Common (trim)
import Data.String (split, Pattern(..))
import Data.String.Regex (regex, split) as R
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse, for_)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Effect (Effect)
import Halogen.HTML (p)


data SnpWeights = SnpWeights
  { snpIds      :: Array String
  , chromosomes :: Array String
  , positions   :: Uint32Array
  , pcWeights   :: Array Float32Array
  , numSnps     :: Int
  , numPCs     :: Int
  }

readSnpWeights :: String -> Effect SnpWeights
readSnpWeights fileContent = do
  let lines = split (Pattern "\n") fileContent
  let numSnps = length lines
  ST.run $ do
    snpIds <- STA.thaw (replicate numSnps "")
    chromosomes <- STA.thaw (replicate numSnps "")
    positions <- empty numSnps
    let (Right wsRegex) = R.regex "\\s+" global
    let (Just firstLine) = head lines
    let firstLineFields = R.split wsRegex (trim firstLine)
    let numPCs = length firstLineFields - 3
    pcWeights <- traverse (\_ -> empty numSnps) (0 .. (numPCs - 1))
    let snpWeights = SnpWeights {
          snpIds = snpIds,
          chromosomes = chromosomes,
          positions = positions,
          pcWeights = pcWeights,
          numSnps = numSnps,
          numPCs = numPCs
        }
    for_ (zip (0 .. (numSnps - 1)) lines) \(Tuple i line) -> do
      let fields = R.split wsRegex (trim line)
      let mChange = do --Maybe monad
            snpId <- fields !! 0
            chrom <- fields !! 1
            pos <- DN.fromString <$> (fields !! 2)
            pcWeights_ <- traverse DN.fromString (drop 3 fields) 
            addSnpWeightEntry snpWeights i snpId chrom pos pcWeights_
      case mChange of
        Just _ -> pure unit
        Nothing -> pure unit -- Skip invalid lines
      pure snpWeights

addSnpWeightEntry :: SnpWeights -> Index -> String -> String -> UInt -> Array Float32 -> Effect Unit
addSnpWeightEntry snpWeights i snpId chrom pos pcWs = do
  set snpWeights.snpIds (Just i) [snpId]
  set snpWeights.chromosomes (Just i) [chrom]
  set snpWeights.positions (Just i) [pos]
  for_ (zip (0 .. (snpWeights.numPCs - 1)) pcWs) \[pcIndex, pcW] -> do
    set snpWeights.pcWeights[pcIndex] (Just i) [pcW]

