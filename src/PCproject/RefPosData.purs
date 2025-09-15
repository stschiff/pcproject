module PCproject.RefPosData where

-- import Prelude
import Data.ArrayBuffer.Types (Float32Array)

type RefPosData =
  { sampleIDs :: Array String
  , popNames :: Array String
  , pcValues :: Float32Array
  , numSamples :: Int
  , numPCs :: Int
  }

foreign import readRefPosData :: String -> RefPosData