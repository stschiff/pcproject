module PCproject.SnpWeights
  ( readSnpWeights
  )
  where

import Prelude

import Control.Monad.ST as ST
import Data.Array ((..), (!!), head, drop, replicate, length, zip)
import Data.Array.ST as STA
import Data.ArrayBuffer.Typed (empty, set, Index, fromArray)
import Data.ArrayBuffer.Types (Float32Array, Uint32Array)
import Data.Either (Either(..))
import Data.Float32 (Float32)
import Data.Int as DI
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as DN
import Data.String.Common (trim)
import Data.String (split, Pattern(..))
import Data.String.Regex (regex, split) as R
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse, for_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple6)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Exception (throwException, throw)


-- Column-wise storage of SNP weights
-- Each SNP has an ID, chromosome, position, and weights for each PC
-- We store columns rather than row-wise structs for better memory- and overall efficiency.
data SnpWeights = SnpWeights
  { snpIds      :: Array String
  , chromosomes :: Array String
  , positions   :: Array Int
  , refAlleles  :: Array String
  , altAlleles  :: Array String
  , pcWeights   :: Array Number
  , numSnps     :: Int
  , numPCs      :: Int
  }

readSnpWeights :: String -> Effect SnpWeights
readSnpWeights fileContent = do
  let lines = split (Pattern "\n") fileContent
  let numSnps = length lines
  wsRegex <- case R.regex "\\s+" global of
    Left err -> throw err
    Right r -> pure r
  firstLineFields <- case head lines of
    Nothing -> throw "snp weight file content is empty"
    Just fl -> pure $ R.split wsRegex <<< trim $ fl
  let numPCs = length firstLineFields - 3
  pure <<< ST.run $ do
    snpIds <- STA.thaw (replicate numSnps "")
    chromosomes <- STA.thaw (replicate numSnps "")
    positions <- STA.thaw (replicate numSnps 0)
    refAlleles <- STA.thaw (replicate numSnps "")
    altAlleles <- STA.thaw (replicate numSnps "")
    pcWeights <- STA.thaw (replicate (numPCs * numSnps) 0.0)
    for_ (0 .. (numSnps - 1)) \i -> do
      let
        maybeValues = do -- Maybe monad
          line <- lines !! i
          let fields = R.split wsRegex (trim line)
          snpId     <- fields !! 0
          chromosome <- fields !! 1
          positionStr <- fields !! 2
          position <- DI.fromString positionStr
          refAllele <- fields !! 3
          altAllele <- fields !! 4
          pcWs <- traverse DN.fromString (drop 5 fields)
          pure (tuple6 snpId chromosome position refAllele altAllele pcWs)
      case maybeValues of
        Nothing -> throw $ "Error parsing line " <> show i <> " in snp weight file"
        Just t -> do
          STA.poke i (get1 t) snpIds
          STA.poke i (get2 t) chromosomes
          STA.poke i (get3 t) positions
          STA.poke i (get4 t) altAlleles
          STA.poke i (get5 t) refAlleles
          for_ (0 .. (numPCs - 1)) \j -> do
            STA.poke (i * numPCs + j) (pcWs !! j) pcWeights
    snpIds'      <- STA.freeze snpIds
    chromosomes' <- STA.freeze chromosomes
    refAlleles'  <- STA.freeze refAlleles
    altAlleles'  <- STA.freeze altAlleles
    positions'   <- STA.freeze positions
    pcWeights'   <- STA.freeze pcWeights
    pure $ SnpWeights { snpIds : snpIds'
      , chromosomes : chromosomes'
      , refAlleles  : refAlleles'
      , altAlleles  : altAlleles'
      , positions   : positions'
      , pcWeights   : pcWeights'
      , numSnps     : numSnps
      , numPCs      : numPCs
      }
