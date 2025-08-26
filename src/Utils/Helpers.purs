module Utils.Helpers where

import Prelude

import Data.Array ((!!), drop)
import Data.Either (Either(..))
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Number as DN
import Data.String.Common (trim)
import Data.String.Regex (regex, split) as R
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse)
import Utils.Types (SnpWeight(..))

readSnpWeightLine :: String -> Maybe SnpWeight
readSnpWeightLine line = do
  wsRegex <- case R.regex "\\s+" global of
    Left _ -> Nothing
    Right r -> Just r
  let fields = R.split wsRegex (trim line)
  snpId <- fields !! 0
  chromosome <- fields !! 1
  position <- fields !! 2 >>= DI.fromString
  pcWeights <- traverse DN.fromString (drop 3 fields)
  pure $ SnpWeight { snpId, chromosome, position, pcWeights }
