module PCproject.RefPosData where

type RefPosData =
  { sampleIDs :: Array String
  , popNames :: Array String
  , pcValues :: Array (Array Number)
  , numSamples :: Int
  , numPCs :: Int
  }

foreign import readRefPosData :: String -> RefPosData