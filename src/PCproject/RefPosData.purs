module PCproject.RefPosData where

type SampleData =
  { sampleID :: String
  , popName :: String
  , popGroup :: String
  , pcValues :: Array Number
  }

type RefPosData =
  { samples :: Array SampleData
  , numSamples :: Int
  , numPCs :: Int
  }

foreign import readRefPosData :: String -> RefPosData