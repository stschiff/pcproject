module PCproject.PlinkData where

import Data.ArrayBuffer.Types (Uint8Array, Uint16Array)

type PlinkBimData =
  { snpIDs :: Array String
  , chromosomes :: Array String
  , positions :: Uint16Array
  , alleles1 :: Array String
  , alleles2 :: Array String
  }

type PlinkFamData =
  { indNames :: Array String
  , popNames :: Array String
  }

type PlinkBedData = Uint8Array

type PlinkData =
  { bimData :: PlinkBimData
  , famData :: PlinkFamData
  , bedData :: PlinkBedData
  , numIndividuals :: Int
  , numSNPs :: Int
  }

foreign import readBimData :: String -> PlinkBimData
foreign import readFamData :: String -> PlinkFamData

checkBedfileMagicBytes :: ArrayBuffer -> Effect Unit
checkBedFileMagicBytes arr = do
  bytes <- whole arr :: Effect Uint8Array
  case take 3 bytes of
    [0b01101100, 0b00011011, 0b00000001] -> pure unit
    _ -> throw "Invalid .bed file: incorrect or missing magic numbers"
