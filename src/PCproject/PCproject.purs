module PCproject.PCproject where

import PCproject.PlinkData (PlinkData)
import PCproject.SnpWeights (SnpWeights)

type ProjectionResult =
    { pcPositions :: Array Number
    , numIndividuals :: Int
    , numPCs :: Int
    , analysedPositions :: Array Int
    , overlappingPositions :: Int
}

foreign import projectPlinkOnWeights :: PlinkData -> SnpWeights -> ProjectionResult
