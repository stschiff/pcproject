module Utils.Helpers where

import Prelude

import Data.Array ((!!), head, drop, replicate)
import Data.ArrayBuffer.Typed (empty)
import Data.Either (Either(..))
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Number as DN
import Data.String.Common (trim)
import Data.String.Regex (regex, split) as R
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse)
import Utils.Types (SnpWeight(..))

readSnpWeightLines :: String -> Effect (Maybe SnpWeights)
readSnpWeightLines fileContent = do
  let lines = split (Pattern "\n") fileContent
  let numSnps = length lines
  let snpIds = replicate numSnps ""
  let chromosomes = replicate numSnps ""
  positions <- empty numSnps
  let wsRegex = case R.regex "\\s+" global of
    Left _ -> Nothing
    Right r -> Just r
  let firstLine 
  let firstLineFields = R.split wsRegex (trim (lines !! 0))
  let numPCs = length firstLineFields - 3

  snpId <- fields !! 0
  chromosome <- fields !! 1
  position <- fields !! 2 >>= DI.fromString
  pcWeights <- traverse DN.fromString (drop 3 fields)
  pure $ SnpWeight { snpId, chromosome, position, pcWeights }
