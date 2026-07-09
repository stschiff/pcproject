module App.ProjChart where

import Prelude

import App.RefChart (tooltipLabelImpl)
import Chartjs (defaultConfig, defaultDataset, defaultOptions)
import Chartjs.Callbacks (defaultTooltipCallbacks, defaultCallbacks)
import Chartjs.Types (defaultInteractionConfig, defaultPluginsConfig, defaultLegendConfig,
        InteractionMode(..), DataPoint(..), ChartType(..), single, css)
import Chartjs.Halogen as HC
import Data.Array ((!!))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import PCproject.RefPosData (RefPosData)
import Type.Proxy (Proxy(..))

type ProjectedSample =
    { sampleID :: String
    , popGroup :: String
    , pcValues :: Array Number
    }

type State =
    { refPosData :: RefPosData
    , projectedSamples :: Array ProjectedSample
    , xPc :: Int
    , yPc :: Int
    }

type Input =
    { refPosData :: RefPosData
    , projectedSamples :: Array ProjectedSample
    }

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
initialState { refPosData, projectedSamples } = { refPosData, projectedSamples, xPc: 1, yPc: 2 }

render :: forall a m . (MonadAff m) => State -> H.ComponentHTML a Slots m
render st =
    let
        toXY :: forall r. { pcValues :: Array Number | r } -> Maybe DataPoint
        toXY sample = XY <$> (sample.pcValues !! (st.xPc - 1)) <*> (sample.pcValues !! (st.yPc - 1))

        -- All reference samples as a single grayed-out background layer. Chart.js draws
        -- datasets in ascending (order, index) order but in *reverse*, so the highest
        -- `order` is drawn first (bottom) and the lowest is drawn last (top) -- hence
        -- giving this dataset the higher order to keep it behind the projected samples.
        backgroundDataset = defaultDataset
            { label = "Reference samples"
            , data = Array.mapMaybe toXY st.refPosData.samples
            , backgroundColor = single (css "rgba(170, 170, 170, 0.5)")
            , pointRadius = single 2.0
            , pointHoverRadius = single 3.0
            , order = Just 1
            }
        -- Left blank so hovering a reference point never surfaces its info -- tooltips
        -- are meant for the projected samples only.
        backgroundLabels = map (const "") st.refPosData.samples

        -- Projected samples all in one flat, undifferentiated dataset for now (no
        -- per-population coloring yet, since PLINK fam files carry only one label
        -- per individual, not a separate finer/coarser grouping like the reference data).
        projDataset = defaultDataset
            { label = "Projected samples"
            , data = Array.mapMaybe toXY st.projectedSamples
            , backgroundColor = single (css "black")
            , pointRadius = single 4.0
            , pointHoverRadius = single 5.0
            , order = Just 0
            }
        projLabels = map (\sample -> sample.sampleID <> " (" <> sample.popGroup <> ")") st.projectedSamples

        chartInput =
            { config : defaultConfig
                { chartType = Scatter
                , datasets = [ backgroundDataset, projDataset ]
                , options =
                    defaultOptions
                        { interaction = Just ( defaultInteractionConfig { mode = Just IMNearest } )
                        , aspectRatio = Just 1.2
                        , plugins = Just ( defaultPluginsConfig { legend = Just ( defaultLegendConfig { display = Just false } ) } )
                        }
                }
            , callbacks : defaultCallbacks
                { tooltipCallbacks = Just (defaultTooltipCallbacks
                    { label = Just (tooltipLabelImpl [ backgroundLabels, projLabels ])
                    })
                }
            }
    in  HH.div_ [ HH.slot_ _chart unit HC.component chartInput ]
