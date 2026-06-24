module App.RefChart where

import Prelude

import Chartjs (defaultConfig, defaultDataset, defaultOptions)
import Chartjs.Callbacks (defaultTooltipCallbacks, defaultCallbacks, TooltipItem)
import Chartjs.Types (defaultInteractionConfig, InteractionMode(..), DataPoint(..), ChartType(..))
import Chartjs.Halogen as HC
import Data.Array ((!!), groupAllBy)
import Data.Array.NonEmpty (head, mapMaybe)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Uncurried (EffectFn1)
import Halogen as H
import Halogen.HTML as HH
import PCproject.RefPosData (RefPosData)
import Type.Proxy (Proxy(..))

foreign import tooltipLabelImpl :: Array (Array String) -> EffectFn1 TooltipItem String

type State =
    { refPosData :: RefPosData
    , xPc :: Int
    , yPc :: Int
    }

type Input = { refPosData :: RefPosData }

type Slots = ( chart :: forall query . H.Slot query HC.Output Unit)

_chart = Proxy :: Proxy "chart"


component :: forall query m o. MonadAff m => H.Component query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

initialState :: Input -> State
initialState { refPosData } = { refPosData, xPc: 1, yPc: 2 }

render :: forall a m . (MonadAff m) => State -> H.ComponentHTML a Slots m
render st =
    let groupedSamples = groupAllBy (\sample1 sample2 -> compare sample1.popGroup sample2.popGroup) st.refPosData.samples
        datasets = do -- list monad
            group <- groupedSamples
            let groupName = (head group).popGroup
                dataPoints = mapMaybe (\sample -> XY <$> (sample.pcValues !! (st.xPc - 1)) <*> (sample.pcValues !! (st.yPc - 1))) group
            pure $ defaultDataset { label = groupName, data = dataPoints }
             nn
        chartInput =
            { config : defaultConfig
                { chartType = Scatter
                , datasets = datasets
                , options =
                    defaultOptions {
                        interaction = Just ( defaultInteractionConfig { mode = Just IMNearest } )
                    }
                }
            , callbacks : defaultCallbacks
                { tooltipCallbacks = Just (defaultTooltipCallbacks
                    { label = Just (tooltipLabelImpl 
                    })
                }
            }
    in  HH.div_ [ HH.slot_ _chart unit HC.component chartInput ]
