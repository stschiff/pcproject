module GenotypeIO where

import Data.ArrayBuffer.Types (ArrayBuffer, Uint32Array, Uint8Array)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)

type PlinkBimData =
  { snpIDs :: Array String
  , chromosomes :: Uint8Array
  , positions :: Uint32Array
  , alleles1 :: Uint8Array
  , alleles2 :: Uint8Array
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

foreign import readBimDataImpl :: EffectFn1 String PlinkBimData
foreign import readFamDataImpl :: EffectFn1 String PlinkFamData
foreign import readBedDataImpl :: EffectFn3 ArrayBuffer Int Int PlinkBedData

readBimData :: String -> Effect PlinkBimData
readBimData bimContent = runEffectFn1 readBimDataImpl bimContent
readFamData :: String -> Effect PlinkFamData
readFamData famContent = runEffectFn1 readFamDataImpl famContent
readBedData :: ArrayBuffer -> Int -> Int -> Effect PlinkBedData
readBedData bedArrayBuffer numSnps numInds = runEffectFn3 readBedDataImpl bedArrayBuffer numSnps numInds
