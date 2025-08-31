module PCproject.SnpWeights where

import Data.ArrayBuffer.Types (Float32Array, Uint16Array)
-- import Prelude

-- import Control.Monad.ST as ST
-- import Data.Array ((..), head, drop, replicate, length, unsafeIndex)
-- import Data.Array.ST as STA
-- import Data.ArrayBuffer.Types (Float32, Uint16Array, Float32Array)
-- import Data.Int as DI
-- import Data.Maybe (Maybe(..))
-- import Data.Number as DN
-- import Data.String (split, Pattern(..))
-- import Data.String.Common (trim)
-- import Data.String.Regex (split) as R
-- import Data.String.Regex.Flags (global)
-- import Data.String.Regex.Unsafe (unsafeRegex)
-- import Data.Traversable (traverse, for_)
-- import Data.TraversableWithIndex (forWithIndex)
-- import Effect (Effect)
-- import Effect.Exception (throw)
-- import Partial (crashWith)
-- import Partial.Unsafe (unsafePartial)


-- Column-wise storage of SNP weights
-- Each SNP has an ID, chromosome, position, and weights for each PC
-- We store columns rather than row-wise structs for better memory- and overall efficiency.
data SnpWeights = SnpWeights
  { snpIds      :: Array String
  , chromosomes :: Array String
  , positions   :: Uint16Array
  , alleles1  :: Array String
  , alleles2  :: Array String
  , pcWeights   :: Float32Array -- Flattened 2D array: numSnps * numPCs
  , numSnps     :: Int
  , numPCs      :: Int
  }

foreign import readSnpWeights :: String -> SnpWeights

-- readSnpWeights :: String -> Effect SnpWeights
-- readSnpWeights fileContent = do
--   let lines = split (Pattern "\n") fileContent
--   let numSnps = length lines
--   let wsRegex = unsafeRegex "\\s+" global
--   firstLineFields <- case head lines of
--     Nothing -> throw "snp weight file content is empty"
--     Just fl -> pure $ R.split wsRegex <<< trim $ fl
--   numPCs <- case length firstLineFields - 5 of
--     n | n <= 0 -> throw "snp weight file must have at least 6 columns (SNP ID, chromosome, position, ref, alt) plus at least one PC weight column"
--     n -> pure n
--   pure <<< unsafePartial $ ST.run do
--       snpIds <- STA.thaw (replicate numSnps "")
--       chromosomes <- STA.thaw (replicate numSnps "")
--       positions <- STA.thaw (replicate numSnps 0)
--       refAlleles <- STA.thaw (replicate numSnps "")
--       altAlleles <- STA.thaw (replicate numSnps "")
--       pcWeights <- STA.thaw (replicate (numPCs * numSnps) 0.0)
--       _ <- forWithIndex lines (\i line -> do
--         let fields = R.split wsRegex (trim line)
--         _ <- STA.poke i (fields `unsafeIndex` 0) snpIds
--         _ <- STA.poke i (fields `unsafeIndex` 1) chromosomes
--         _ <- case DI.fromString (fields `unsafeIndex` 2) of
--           Nothing -> crashWith $ "Error parsing position in line " <> show i <> " in snp weight file"
--           Just p  -> STA.poke i p positions
--         _ <- STA.poke i (fields `unsafeIndex` 3) refAlleles
--         _ <- STA.poke i (fields `unsafeIndex` 4) altAlleles
--         case traverse DN.fromString (drop 5 fields) of
--           Nothing -> crashWith $ "Error parsing PC weights in line " <> show i <> " in snp weight file"
--           Just pcWs -> for_ (0 .. (numPCs - 1)) (\j -> STA.poke (i * numPCs + j) (pcWs `unsafeIndex` j) pcWeights)
--       )
--       snpIds'      <- STA.freeze snpIds
--       chromosomes' <- STA.freeze chromosomes
--       refAlleles'  <- STA.freeze refAlleles
--       altAlleles'  <- STA.freeze altAlleles
--       positions'   <- STA.freeze positions
--       pcWeights'   <- STA.freeze pcWeights
--       pure $ SnpWeights
--         { snpIds      : snpIds'
--         , chromosomes : chromosomes'
--         , positions   : positions'
--         , refAlleles  : refAlleles'
--         , altAlleles  : altAlleles'
--         , pcWeights   : pcWeights'
--         , numSnps     : numSnps
--         , numPCs      : numPCs
--         }
