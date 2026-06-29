module PCproject.SnpWeights where

import Data.ArrayBuffer.Types (Float32Array, Uint32Array)

-- Column-wise storage of SNP weights
-- Each SNP has an ID, chromosome, position, and weights for each PC
-- We store columns rather than row-wise structs for better memory- and overall efficiency.
type SnpWeights =
  { snpIDs      :: Array String
  , chromosomes :: Uint8Array
  , positions   :: Uint32Array
  , alleles1  :: Uint8Array
  , alleles2  :: Uint8Array
  , pcWeights   :: Float32Array -- Flattened 2D array: numSnps * numPCs
  , frequencies :: Float32Array
  , numSNPs     :: Int
  , numPCs      :: Int
  }

foreign import readSnpWeights :: String -> SnpWeights
