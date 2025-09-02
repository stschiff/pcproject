module PCproject.PCproject where

import Prelude

type ProjectionResult =
    { pcPositions :: Array Number
    , numIndividuals :: Int
    , numPCs :: Int
    , analysedPositions :: Array Int
    , overlappingPositions :: Int
}

foreign import projectPlinkOnWeights :: PlinkData -> SnpWeights -> ProjectionResult
