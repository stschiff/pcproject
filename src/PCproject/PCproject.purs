module PCproject.PCproject where

import Data.ArrayBuffer.Types (Uint8Array, Float32Array)
import Effect.Uncurried (EffectFn2, EffectFn4, runEffectFn2, runEffectFn4)

import PCproject.PlinkData (PlinkData, PlinkBimData)
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

getOverlapMasks :: BimData -> SnpWeights -> OverlapMasks

reducePcWeights :: SnpWeights -> OverlapMasks -> SnpWeights

extractAndTransposeGenotypes :: Uint8Array -> number -> number -> OverlapMasks -> Uint8Array

projectSamples :: Uint8Array -> Float32Array -> Float32Array -> number -> number -> number -> number -> Array Number -> Array ProjectionResult
