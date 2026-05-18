module App.RefChart where

import Prelude

import Chartjs (defaultConfig, defaultDataset, simpleInput)
import Chartjs.Halogen as HC
import Chartjs.Types (DataPoint(..), ChartType(..))
import Data.Array ((!!))
import Data.Traversable (for)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import PCproject.RefPosData (RefPosData)
import Type.Proxy (Proxy(..))

type State =
    { refPosData :: RefPosData
    , xPc :: Int
    , yPc :: Int
    }

type Input = { refPosData :: RefPosData }

type Slots = ( chart :: forall query . H.Slot query HC.Output Unit)

_chart = Proxy :: Proxy "chart"

data Action
  = ReceivedRefPosDataEvent RefPosData
  | SetXPC Int
  | SetYPC Int

component :: forall query m o. MonadAff m => H.Component query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { receive = \{ refPosData } -> Just $ ReceivedRefPosDataEvent refPosData
        }
    }

initialState :: Input -> State
initialState { refPosData } = { refPosData, xPc: 1, yPc: 2 }

render :: forall m . (MonadAff m) => State -> H.ComponentHTML Action Slots m
render st =
    let maybePcData = for st.refPosData.pcValues (\row ->
            XY <$> (row !! (st.xPc - 1)) <*> (row !! (st.yPc - 1))
        )
        pcData = case maybePcData of
            Just dataPoints -> dataPoints
            Nothing -> []
        chartInput = simpleInput $ defaultConfig
            { chartType = Scatter
            , datasets = [ defaultDataset { label = "hello", data = pcData } ]
            }
    in  HH.div_ [ HH.slot_ _chart unit HC.component chartInput ]
