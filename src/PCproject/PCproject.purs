module PCproject.PCproject where

import Data.ArrayBuffer.Types (Uint8Array, Float32Array)
import Effect.Uncurried (EffectFn2, EffectFn4, runEffectFn2, runEffectFn4)

import PCproject.PlinkData (PlinkData, PlinkBimData)
import PCproject.SnpWeights (SnpWeights)

type OverlapResult =
    { snpWeightOverlap :: Float32Array
    , plinkMask :: Uint8Array
    , flipMask :: Uint8Array
    , removedStrandAmbiguous :: Int
    , removedInconsistent :: Int
    }

type ProjectionIndividualResult =
    { pcPositions :: Array Number
    , nonMissingPositions :: Int
    }

foreign import getOverlapMasksImpl :: EffectFn2 PlinkBimData SnpWeights OverlapResult
getOverlapMasks :: PlinkData -> SnpWeights -> OverlapResult
getOverlapMasks plinkData snpWeights = runEffectFn2 getOverlapMasksImpl plinkData snpWeights

foreign import projectImpl :: EffectFn4 Float32Array Uint8Array Uint8Array Uint8Array ProjectionIndividualResult

project :: OverlapResult -> Uint8Array -> ProjectionIndividualResult
project { snpWeightOverlap, plinkMask, flipMask } genoVec =
    runEffectFn4 projectImpl snpWeightOverlap plinkMask flipMask genoVec
