module App.ProjectionMonitor where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H

import PCproject.PlinkData (PlinkData)
import PCproject.SnpWeights (SnpWeights)

type State =
    { input          :: Input
    , datasetOverlap :: Maybe Int
    , removedSnpCount :: Maybe Int
    }

type Input = { snpWeights :: SnpWeights, plinkData :: PlinkData }

type Output = { projectionCoordinates :: Array (Array Number) }

data Action = Receive Input | RunProjection

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, datasetOverlap: Nothing, removedSnpCount: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just RunProjection
        , receive = Just <<< Receive
        }
    }

handleAction :: forall slots m. MonadAff m => Action -> H.HalogenM State Action slots Output m Unit
handleAction (Receive input) = do
    H.modify_ \st -> st { input = input }
    handleAction RunProjection
handleAction RunProjection = do
    { input : { snpWeights, plinkData } } <- H.get
    compute


render :: forall m . (MonadAff m) => State -> H.ComponentHTML Action Slots m
render st = _
