module PCproject.PlinkData where

import Prelude

import Data.UInt (toInt)
import Data.ArrayBuffer.Typed (whole, (!))
import Data.ArrayBuffer.Types (Uint8Array, Uint32Array, ArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)

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

type PlinkBedData = ArrayBuffer

type PlinkData =
  { bimData :: PlinkBimData
  , famData :: PlinkFamData
  , bedData :: PlinkBedData
  , numIndividuals :: Int
  , numSNPs :: Int
  }

foreign import readBimData :: String -> PlinkBimData
foreign import readFamData :: String -> PlinkFamData

checkBedFileMagicBytes :: ArrayBuffer -> Effect Boolean
checkBedFileMagicBytes arr = do
  bytes <- whole arr :: Effect Uint8Array
  maybeFirstThree <- sequence [bytes ! 0, bytes ! 1, bytes ! 2]
  case map (map toInt) $ sequence maybeFirstThree of
    Just [108, 27, 1] -> pure true -- magic bytes are [0b01101100, 0b00011011, 0b00000001]
    _ -> pure false