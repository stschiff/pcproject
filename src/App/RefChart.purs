module App.RefChart where

import Prelude

import Chartjs (defaultConfig, defaultDataset, simpleInput)
import Chartjs.Halogen as HC
import Chartjs.Types (DataPoint(..), ChartType(..))
import Data.Array ((!!), groupAllBy)
import Data.Array.NonEmpty (head, toArray)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
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
    let groupedSamples = groupAllBy (\sample1 sample2 -> compare sample1.popName sample2.popName) st.refPosData.samples
        datasets = do -- list monad
            group <- groupedSamples
            let groupName = (head group).popName
                maybeDataPoints = traverse (\sample -> XY <$> (sample.pcValues !! (st.xPc - 1)) <*> (sample.pcValues !! (st.yPc - 1))) group
            case maybeDataPoints of
                Just points -> pure $ defaultDataset { label = groupName, data = toArray points }
                Nothing -> []
        chartInput = simpleInput $ defaultConfig
            { chartType = Scatter
            , datasets = datasets
            }
    in  HH.div_ [ HH.slot_ _chart unit HC.component chartInput ]
