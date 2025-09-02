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