module PCproject.PlinkData where

import Data.ArrayBuffer.Types (Uint8Array, Uint16Array)

data PlinkBimData = PlinkBimData
  { snpIds :: Array String
  , chromosomes :: Array String
  , positions :: Uint16Array
  , alleles1 :: Array String
  , alleles2 :: Array String
  }

data PlinkFamData = PlinkFamData
  { indNames :: Array String
  , popNames :: Array String
  }

type PlinkBedData = Uint8Array

data PlinkData = PlinkData
  { bimData :: PlinkBimData
  , famData :: PlinkFamData
  , bedData :: PlinkBedData
  , numIndividuals :: Int
  , numSnps :: Int
  }

foreign import readBimData :: String -> PlinkBimData
foreign import readFamData :: String -> PlinkFamData