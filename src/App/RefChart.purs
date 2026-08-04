module App.RefChart where

import Prelude

import Chartjs (defaultConfig, defaultDataset, defaultOptions)
import Chartjs.Callbacks (defaultTooltipCallbacks, defaultCallbacks, TooltipItem)
import Chartjs.Halogen as HC
import Chartjs.Types (ChartType(..), DataPoint(..), InteractionMode(..),
                      defaultInteractionConfig, defaultScaleConfig,
                      defaultScaleTitleConfig)
import Data.Array ((!!), groupAllBy)
import Data.Array.NonEmpty (head, mapMaybe, toArray)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Uncurried (EffectFn1)
import Foreign.Object (fromFoldable)
import Halogen as H
import Halogen.HTML as HH
import PCproject.RefPosData (RefPosData)
import Type.Proxy (Proxy(..))

foreign import tooltipLabelImpl :: Array (Array String) -> EffectFn1 TooltipItem String

type State =
    { refPosData :: RefPosData
    , xPCindex :: Int
    , yPCindex :: Int
    }

type Input = State

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
initialState inputState = inputState

render :: forall a m . (MonadAff m) => State -> H.ComponentHTML a Slots m
render st =
    let groupedSamples = groupAllBy (\sample1 sample2 -> compare sample1.popGroup sample2.popGroup) st.refPosData.samples
        datasets = do -- list monad
            group <- groupedSamples
            let groupName = (head group).popGroup
                dataPoints = mapMaybe (\sample -> XY <$> (sample.pcValues !! (st.xPCindex - 1)) <*> (sample.pcValues !! (st.yPCindex - 1))) group
            pure $ defaultDataset { label = groupName, data = dataPoints }
        labels = map (\sampleGroup -> map (\sample -> sample.popName) (toArray sampleGroup)) groupedSamples
        chartInput =
            { config : defaultConfig
                { chartType = Scatter
                , datasets = datasets
                , options =
                    defaultOptions
                        { interaction = Just ( defaultInteractionConfig { mode = Just IMNearest } )
                        , aspectRatio = Just 1.2
                        , scales = Just $ fromFoldable
                            [ Tuple "x" defaultScaleConfig
                                { title = Just defaultScaleTitleConfig { display = Just true, text = Just $ "PC" <> show st.xPCindex } }
                            , Tuple "y" defaultScaleConfig
                                { title = Just defaultScaleTitleConfig { display = Just true, text = Just $ "PC" <> show st.yPCindex } }
                            ]
                        }
                }
            , callbacks : defaultCallbacks
                { tooltipCallbacks = Just (defaultTooltipCallbacks
                    { label = Just (tooltipLabelImpl labels)
                    })
                }
            }
    in  HH.div_ [ HH.slot_ _chart unit HC.component chartInput ]
