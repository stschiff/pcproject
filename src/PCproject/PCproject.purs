module PCproject.PCproject where

import Data.ArrayBuffer.Types (Uint8Array, Float32Array)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, EffectFn4, EffectFn6, runEffectFn2, runEffectFn4, runEffectFn6)
import Prelude

import PCproject.PlinkData (PlinkBimData)
import PCproject.SnpWeights (SnpWeights)

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
    }

foreign import getOverlapMasksImpl :: EffectFn2 PlinkBimData SnpWeights OverlapMasks
getOverlapMasks :: forall m. MonadEffect m => PlinkBimData -> SnpWeights -> m OverlapMasks
getOverlapMasks sampleBimData snpWeights =
    liftEffect $ runEffectFn2 getOverlapMasksImpl sampleBimData snpWeights

foreign import reducePcWeightsImpl :: EffectFn2 SnpWeights OverlapMasks SnpWeights
reducePcWeights :: forall m. MonadEffect m => SnpWeights -> OverlapMasks -> m SnpWeights
reducePcWeights snpWeights overlap =
    liftEffect $ runEffectFn2 reducePcWeightsImpl snpWeights overlap

foreign import extractAndTransposeGenotypesImpl :: EffectFn4 Uint8Array Int Int OverlapMasks Uint8Array
extractAndTransposeGenotypes :: forall m. MonadEffect m => Uint8Array -> Int -> Int -> OverlapMasks -> m Uint8Array
extractAndTransposeGenotypes plinkBedDat numSNPs numInds overlap =
    liftEffect $ runEffectFn4 extractAndTransposeGenotypesImpl plinkBedDat numSNPs numInds overlap

foreign import projectSamplesImpl :: EffectFn6 Uint8Array Float32Array Float32Array Int Int PCAparams (Array ProjectionResult)
projectSamples :: forall m. MonadEffect m => Uint8Array -> Float32Array -> Float32Array -> Int -> Int -> PCAparams -> m (Array ProjectionResult)
projectSamples transposedGenotypeMatrix pcWeight frequencies numInds numPCs params =
    liftEffect $ runEffectFn6 projectSamplesImpl transposedGenotypeMatrix pcWeight frequencies numInds numPCs params