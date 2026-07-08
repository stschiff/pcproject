module App.Interface where

import Prelude

import App.RefChart as RefChart
import App.UserInputComponent as UserInputComponent
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.File.File (File)

import PCproject.PCproject (ProjectionResult, projectSamples, PCAparams,
        getOverlapMasks, reducePcWeights, extractAndTransposeGenotypes,
        OverlapMasks(..))
import PCproject.PlinkData (PlinkData)
import PCproject.RefPosData (RefPosData, readRefPosData)
import PCproject.SnpWeights (SnpWeights, readSnpWeights)

data PlinkFileSpec = PlinkFileSpec File File File

type State =
  { snpWeights :: Maybe SnpWeights
  , userData :: Maybe PlinkData
  , refData :: Maybe RefPosData
  , pcaParams :: Maybe PCAparams
  , projectionResults :: Maybe (Array ProjectionResult)
  , projectionRunning :: Boolean
  , errorNote :: Maybe String
  , overlap :: Maybe OverlapMasks
  }

data Action
  = LoadRefData
  | GotUserData PlinkData
  | RunProjection PlinkData SnpWeights PCAparams

type Slots = ( refChart :: forall q o . H.Slot q o Unit
             , userInputComponent :: forall q . H.Slot q UserInputComponent.Output Unit
             )

_refChart = Proxy :: Proxy "refChart"
_userInputComponent = Proxy :: Proxy "userInputComponent"

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just LoadRefData
        }
    }

render :: forall m . (MonadAff m) => State -> H.ComponentHTML Action Slots m
render st =
    HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.h1 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-1"] ]
            [ HH.text "PC Projection Tool" ]
        , HH.div [ HP.classes [ HH.ClassName "columns" ] ]
            [ HH.div [ HP.classes [ HH.ClassName "column" ] ] [ refDataBox st ]
            , HH.div [ HP.classes [ HH.ClassName "column" ] ] [ projectionMonitor st ]
            , HH.div [ HP.classes [ HH.ClassName "column" ] ]
                [ HH.slot _userInputComponent unit UserInputComponent.component unit GotUserData ]
            ]
        , HH.div [ HP.classes [ HH.ClassName "columns" ] ]
            [ HH.div [ HP.classes [ HH.ClassName "column" ] ] [ refChartBox st ]
            , HH.div [ HP.classes [ HH.ClassName "column" ] ] [ projChartBox st ]
            ]
        ]

refDataBox :: forall slots m . (MonadAff m) => State -> H.ComponentHTML Action slots m
refDataBox st = 
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Reference Data" ]
        , case st.snpWeights of
            Nothing -> HH.div_ [ HH.text "Loading weight file...", HH.br_ ]
            Just sw -> HH.div_
                [ HH.text $ "Selected weight file with SNPs: " <>
                    show sw.numSNPs <> ", PCs: " <> show sw.numPCs, HH.br_
                ]
        , case st.refData of
            Nothing -> HH.div_ [ HH.text "Loading reference position file...", HH.br_ ]
            Just rd -> HH.div_
                [ HH.text $ "Reference Position Data loaded. Samples: " <>
                    show rd.numSamples <> ", PCs: " <> show rd.numPCs, HH.br_
                ]
        , case st.errorNote of
            Nothing -> HH.text ""
            Just errMsg -> HH.div_ [ HH.text $ "Error: " <> errMsg, HH.br_ ]
        ]

projectionMonitor :: forall slots m . (MonadAff m) => State -> H.ComponentHTML Action slots m
projectionMonitor st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Projection Monitor" ]
        , if st.projectionRunning then
            HH.div_ [ HH.text "Projection is running..." ]
          else
            HH.text ""
        , case st.snpWeights /\ st.userData /\ st.pcaParams of
            Just sw /\ Just pd /\ Just pp -> 
                if st.projectionRunning then
                    HH.div [ HP.classes [ HH.ClassName "control" ] ]
                        [ HH.button
                            [ HP.classes [ HH.ClassName "button", HH.ClassName "is-primary", HH.ClassName "is-loading" ] ]
                            [ HH.text "Running" ]
                        ]
                else
                    HH.div [ HP.classes [ HH.ClassName "control" ] ]
                        [ HH.button
                            [ HP.classes [ HH.ClassName "button", HH.ClassName "is-primary"]
                            , HE.onClick (\_ -> RunProjection pd sw pp)
                            ]
                            [ HH.text "Run Projection" ]
                        ]
            _ ->
                HH.text ""
        , case st.overlap of
            Nothing -> HH.text ""
            Just overlap ->
                HH.div_
                    [ HH.text $ "Overlap Masks: "
                        <> "Included SNPs: " <> show overlap.nrIncluded
                        <> ", Strand Ambiguous Removed: " <> show overlap.removedStrandAmbiguous
                        <> ", Inconsistent Removed: " <> show overlap.removedInconsistent
                        <> ", To Be Flipped: " <> show overlap.nrToBeFlipped
                    ]
        , case st.projectionResults of
            Nothing -> HH.text ""
            Just pr -> HH.div_
                [ HH.text $ "Projection done" ]
        ]

refChartBox :: forall action m . (MonadAff m) => State -> H.ComponentHTML action Slots m
refChartBox st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Reference Data Chart" ]
        , case st.refData of
            Nothing -> HH.text ""
            Just rd -> HH.div_
                [ HH.slot_ _refChart unit RefChart.component { refPosData:  rd } ]
        ]

projChartBox :: forall action slots m . (MonadAff m) => State -> H.ComponentHTML action slots m
projChartBox _ =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Projection Results Chart" ]
        , HH.text "Projection results chart will be displayed here after running the projection."
        ]

initialState :: forall input. input -> State
initialState = const
    { snpWeights : Nothing
    , userData : Nothing
    , refData : Nothing
    , pcaParams : Nothing
    , projectionResults : Nothing
    , projectionRunning : false
    , errorNote : Nothing
    , overlap : Nothing
    }

handleAction :: forall output slots m. MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction LoadRefData = do
    f1 <- H.liftAff $ fetch "./assets/Joscha_HiRes_WestEurasia_weights_with_freqs.txt" {}
    if f1.ok
        then do
            content <- H.liftAff $ f1.text
            let snpWeightData = readSnpWeights content
            H.modify_ _ { snpWeights = Just snpWeightData }
        else
            H.modify_ _ { errorNote = Just "Failed to load weight data", snpWeights = Nothing}
    f2 <- H.liftAff $ fetch "./assets/Joscha_HiRes_WestEurasia_evec_with_groups.tsv" {}
    if f2.ok
        then do
            content <- H.liftAff $ f2.text
            let refData = readRefPosData content
            H.modify_ _ { refData = Just refData }
        else
            H.modify_ _ { errorNote = Just "Failed to load reference data", refData = Nothing}
    f3 <- H.liftAff $ fetch "./assets/Joscha_HiRes_WestEurasia_parameters.json" {}
    if f3.ok
        then do
            pcaParams <- H.liftAff $ fromJson f3.json
            liftEffect <<< log $ "Loaded PCA parameters: " <> show pcaParams
            H.modify_ _ { pcaParams = Just pcaParams }
        else
            H.modify_ _ { errorNote = Just "Failed to load PCA parameters", pcaParams = Nothing}
            

handleAction (GotUserData pd) = do
    H.modify_ _ { userData = Just pd, errorNote = Nothing }

handleAction (RunProjection pd sw pp) = do
    H.modify_ _ { projectionRunning = true, projectionResults = Nothing }
    overlap <- liftEffect $ getOverlapMasks pd.bimData sw
    H.modify_ _ { overlap = Just overlap }
    reducedSnpWeights <- liftEffect $ reducePcWeights sw overlap
    genotypes <- liftEffect $ extractAndTransposeGenotypes pd.bedData pd.numSNPs pd.numIndividuals overlap
    pResults <- liftEffect $ projectSamples genotypes reducedSnpWeights.pcWeights reducedSnpWeights.frequencies
        pd.numIndividuals reducedSnpWeights.numPCs pp
    H.modify_ _ { projectionRunning = false, projectionResults = Just pResults }
    pure unit
