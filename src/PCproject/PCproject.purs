module PCproject.PCproject where

import Data.ArrayBuffer.Types (Uint8Array, Float32Array)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn4, EffectFn8, runEffectFn2, runEffectFn4, runEffectFn8)

import PCproject.PlinkData (PlinkBimData)
import PCproject.SnpWeights (SnpWeights)

type OverlapMasks = {
    snpWeightMask :: Uint8Array,
    plinkMask :: Uint8Array,
    flipMask :: Uint8Array,
    removedStrandAmbiguous :: Number,
    removedInconsistent :: Number,
    nrIncluded :: Number,
    nrToBeFlipped :: Number
}

type ProjectionResult = {
    pcCoordinates :: Array Number,
    nonMissingCount :: Number
}

foreign import getOverlapMasksImpl :: EffectFn2 PlinkBimData SnpWeights OverlapMasks
getOverlapMasks :: PlinkBimData -> SnpWeights -> Effect OverlapMasks
getOverlapMasks sampleBimData snpWeights =
    runEffectFn2 getOverlapMasksImpl sampleBimData snpWeights

foreign import reducePcWeightsImpl :: EffectFn2 SnpWeights OverlapMasks SnpWeights
reducePcWeights :: SnpWeights -> OverlapMasks -> Effect SnpWeights
reducePcWeights snpWeights overlap =
    runEffectFn2 reducePcWeightsImpl snpWeights overlap

foreign import extractAndTransposeGenotypesImpl :: EffectFn4 Uint8Array Number Number OverlapMasks Uint8Array
extractAndTransposeGenotypes :: Uint8Array -> Number -> Number -> OverlapMasks -> Effect Uint8Array
extractAndTransposeGenotypes plinkBedDat numSNPs numInds overlap =
    runEffectFn4 extractAndTransposeGenotypesImpl plinkBedDat numSNPs numInds overlap

foreign import projectSamplesImpl :: EffectFn8 Uint8Array Float32Array Float32Array Number Number Number Number (Array Number) (Array ProjectionResult)
projectSamples :: Uint8Array -> Float32Array -> Float32Array -> Number -> Number -> Number -> Number -> Array Number -> Effect (Array ProjectionResult)
projectSamples transposedGenotypeMatrix pcWeight frequencies numInds numPCs nScale yScale eigenValues =
    runEffectFn8 projectSamplesImpl transposedGenotypeMatrix pcWeight frequencies numInds numPCs nScale yScale eigenValues