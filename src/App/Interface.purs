module App.Interface where

import Prelude

import Data.Array (length, zipWith)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
-- import Effect.Console (log)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

import App.ProjChart as ProjChart
import App.RefChart as RefChart
import App.UserInputComponent as UserInputComponent
import App.Utils (RemoteData(..))

import PCproject.PCproject (ProjectionResult, projectSamples, PCAparams,
        getOverlapMasks, reducePcWeights, extractAndTransposeGenotypes,
        OverlapMasks)
import PCproject.PlinkData (PlinkData)
import PCproject.RefPosData (RefPosData, readRefPosData)
import PCproject.SnpWeights (SnpWeights, readSnpWeights)

type ReferenceBundle = {
  snpWeights :: SnpWeights,
  refPosData :: RefPosData,
  pcaParams :: PCAparams
}

type ProjectionBundle = {
  projectionResults :: Array ProjectionResult,
  overlapReport :: OverlapMasks
}

type State =
  { refBundle :: RemoteData String ReferenceBundle
  , userData :: Maybe PlinkData
  , projectionResults :: RemoteData String ProjectionBundle
  }

data Action
  = LoadRefData
  | GotUserData PlinkData
  | RunProjection

type Slots = ( refChart :: forall q o . H.Slot q o Unit
             , projChart :: forall q o . H.Slot q o Unit
             , userInputComponent :: forall q . H.Slot q UserInputComponent.Output Unit
             )

_refChart = Proxy :: Proxy "refChart"
_projChart = Proxy :: Proxy "projChart"
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
        , case st.refBundle of
            NotAsked -> HH.div_ [ HH.text "No reference data yet", HH.br_ ]
            Loading -> HH.div_ [ HH.text "Loading refernece data...", HH.br_ ]
            Failure err -> HH.div_ [ HH.text $ "Error loading refernece bundle: " <> err, HH.br_ ]
            Success rb -> HH.div_
                [ HH.text $ "Selected reference data with " <>
                    show rb.snpWeights.numSNPs <> " SNPs for " <>
                    show rb.snpWeights.numPCs <> " PCs and " <>
                    show rb.refPosData.numSamples <> " individuals", HH.br_
                ]
        ]

projectionMonitor :: forall slots m . (MonadAff m) => State -> H.ComponentHTML Action slots m
projectionMonitor st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Projection Monitor" ]
        , case st.projectionResults of
            NotAsked -> HH.text "No projection performed yet"
            Loading -> HH.text "Projection running..."
            Failure err -> HH.text $ "Error during projection: " <> err
            Success pr -> HH.div_
                [ HH.text "Projection completed successfully"
                , HH.text $ "Number of samples projected: " <> show (length pr.projectionResults)
                , HH.text $ "Overlap Masks: "
                        <> "Included SNPs: " <> show pr.overlapReport.nrIncluded
                        <> ", Strand Ambiguous Removed: " <> show pr.overlapReport.removedStrandAmbiguous
                        <> ", Inconsistent Removed: " <> show pr.overlapReport.removedInconsistent
                        <> ", To Be Flipped: " <> show pr.overlapReport.nrToBeFlipped
                ]
        ]

refChartBox :: forall action m . (MonadAff m) => State -> H.ComponentHTML action Slots m
refChartBox st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Reference Data Chart" ]
        , case st.refBundle of
            Success rb -> HH.div_
                [ HH.slot_ _refChart unit RefChart.component { refPosData:  rb.refPosData } ]
            _ -> HH.text ""
        ]

projChartBox :: forall action m . (MonadAff m) => State -> H.ComponentHTML action Slots m
projChartBox st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Projection Results Chart" ]
        , case st.refBundle /\ st.userData /\ st.projectionResults of
            Success rb /\ Just pd /\ Success pr -> HH.div_
                [ HH.slot_ _projChart unit ProjChart.component
                    { refPosData: rb.refPosData
                    , projectedSamples: toProjectedSamples pd pr.projectionResults
                    }
                ]
            _ -> HH.text "Projection results chart will be displayed here after running the projection."
        ]

toProjectedSamples :: PlinkData -> Array ProjectionResult -> Array ProjChart.ProjectedSample
toProjectedSamples pd results =
    zipWith (\(Tuple sampleID popGroup) pr -> { sampleID, popGroup, pcValues: pr.pcCoordinates })
        (zipWith Tuple pd.famData.indNames pd.famData.popNames)
        results

initialState :: forall input. input -> State
initialState = const
    { refBundle : NotAsked
    , projectionResults : NotAsked
    , userData : Nothing
    }

handleAction :: forall output slots m. MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction LoadRefData = do
    f1 <- H.liftAff $ fetch "./assets/Joscha_HiRes_WestEurasia_weights_with_freqs.txt" {}
    f2 <- H.liftAff $ fetch "./assets/Joscha_HiRes_WestEurasia_evec_with_groups.tsv" {}
    f3 <- H.liftAff $ fetch "./assets/Joscha_HiRes_WestEurasia_parameters.json" {}
    if f1.ok 
        then if f2.ok
            then if f3.ok
                then do
                    snpWeights <- readSnpWeights <$> H.liftAff f1.text
                    refPosData <- readRefPosData <$> H.liftAff f2.text
                    pcaParams <- H.liftAff $ fromJson f3.json
                    H.modify_ _ { refBundle = Success { snpWeights, refPosData, pcaParams } }
                    handleAction RunProjection
                else
                    H.modify_ _ { refBundle = Failure "Failed to load PCA parameters file"}
            else
                H.modify_ _ { refBundle = Failure "Failed to load reference position data file"}
        else
            H.modify_ _ { refBundle = Failure "Failed to load weight data file"}

handleAction (GotUserData pd) = do
    H.modify_ _ { userData = Just pd }
    handleAction RunProjection

handleAction RunProjection = do
    st <- H.get
    case st.refBundle /\ st.userData of
        Success rb /\ Just pd -> do
            H.modify_ _ { projectionResults = Loading }
            nextAnimationFrame -- hack to get the Spinner running
            nextAnimationFrame
            overlap <- liftEffect $ getOverlapMasks pd.bimData rb.snpWeights
            reducedSnpWeights <- liftEffect $ reducePcWeights rb.snpWeights overlap
            genotypes <- liftEffect $ extractAndTransposeGenotypes pd.bedData pd.numSNPs pd.numIndividuals overlap
            pResults <- liftEffect $ projectSamples genotypes reducedSnpWeights.pcWeights reducedSnpWeights.frequencies
                pd.numIndividuals reducedSnpWeights.numPCs rb.pcaParams
            H.modify_ _ { projectionResults = Success { projectionResults: pResults, overlapReport: overlap } }
        _ -> H.modify_ _ { projectionResults = NotAsked }

-- This is just a hack for now, to make sure the spinner starts running while the synchronous projection computation runs.
nextAnimationFrame :: forall m. MonadAff m => m Unit
nextAnimationFrame = liftAff $ makeAff \callback -> do
  win <- window
  _ <- requestAnimationFrame (callback (Right unit)) win
  pure nonCanceler