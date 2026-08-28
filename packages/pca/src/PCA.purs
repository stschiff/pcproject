module PCA
  ( OverlapMasks
  , ProjectionResult
  , PCAparams
  , SampleData
  , RefPosData
  , SnpWeights
  , getOverlapMasks
  , reducePcWeights
  , extractAndTransposeGenotypes
  , projectSamples
  , readRefPosData
  , readSnpWeights
  ) where

import Data.ArrayBuffer.Types (Uint8Array, Uint32Array, Float32Array)
import Effect.Uncurried (EffectFn2, EffectFn4, EffectFn6, runEffectFn2, runEffectFn4, runEffectFn6)
import Effect (Effect)
import GenotypeIO (PlinkBimData)

type OverlapMasks = {
    snpWeightMask :: Uint8Array,
    plinkMask :: Uint8Array,
    flipMask :: Uint8Array,
    removedStrandAmbiguous :: Int,
    removedInconsistent :: Int,
    nrIncluded :: Int,
    nrToBeFlipped :: Int
}

type ProjectionResult = {
    pcCoordinates :: Array Number,
    nonMissingCount :: Int
}

type PCAparams =
    { yScale      :: Number
    , nScale      :: Number
    , eigenValues :: Array Number
    , defaultX    :: Int
    , defaultY    :: Int
    }

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

foreign import getOverlapMasksImpl :: EffectFn2 PlinkBimData SnpWeights OverlapMasks
getOverlapMasks :: PlinkBimData -> SnpWeights -> Effect OverlapMasks
getOverlapMasks sampleBimData snpWeights =
    runEffectFn2 getOverlapMasksImpl sampleBimData snpWeights

foreign import reducePcWeightsImpl :: EffectFn2 SnpWeights OverlapMasks SnpWeights
reducePcWeights :: SnpWeights -> OverlapMasks -> Effect SnpWeights
reducePcWeights snpWeights overlap =
    runEffectFn2 reducePcWeightsImpl snpWeights overlap

foreign import extractAndTransposeGenotypesImpl :: EffectFn4 Uint8Array Int Int OverlapMasks Uint8Array
extractAndTransposeGenotypes :: Uint8Array -> Int -> Int -> OverlapMasks -> Effect Uint8Array
extractAndTransposeGenotypes plinkBedDat numSNPs numInds overlap =
    runEffectFn4 extractAndTransposeGenotypesImpl plinkBedDat numSNPs numInds overlap

foreign import projectSamplesImpl :: EffectFn6 Uint8Array Float32Array Float32Array Int Int PCAparams (Array ProjectionResult)
projectSamples :: Uint8Array -> Float32Array -> Float32Array -> Int -> Int -> PCAparams -> Effect (Array ProjectionResult)
projectSamples transposedGenotypeMatrix pcWeight frequencies numInds numPCs params =
    runEffectFn6 projectSamplesImpl transposedGenotypeMatrix pcWeight frequencies numInds numPCs params

foreign import readRefPosData :: String -> RefPosData
foreign import readSnpWeights :: String -> SnpWeights
