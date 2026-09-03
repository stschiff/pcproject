module App.Interface where

import Prelude

import Data.Array (length, zipWith, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith, replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (attempt, makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
-- import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

import App.Download (downloadCSV)
import App.ProjChart as ProjChart
import App.RefChart as RefChart
import App.UserInputComponent as UserInputComponent
import App.Utils (RemoteData(..))

import Data.Map as Map

import PCA (ProjectionResult, projectSamples, ReferenceBundle, RefPosData,
        getOverlapMasks, reducePcWeights, extractAndTransposeGenotypes,
        OverlapMasks)
import PCA.Assets (defaultFetcher, loadReferenceBundle, panels)
import GenotypeIO (PlinkData)

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
  | DownloadRefPositions
  | DownloadProjectedPositions

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

externalLink :: forall action slots m. String -> String -> H.ComponentHTML action slots m
externalLink url label =
    HH.a [ HP.href url, HP.target "_blank", HP.rel "noopener noreferrer" ] [ HH.text label ]

render :: forall m . (MonadAff m) => State -> H.ComponentHTML Action Slots m
render st =
    HH.div_
        [ HH.section [ HP.classes [ HH.ClassName "section" ] ]
            [ HH.h1 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-1"] ]
                [ HH.text "PCproject" ]
            , HH.h2 [ HP.classes [ HH.ClassName "subtitle", HH.ClassName "is-4" ] ]
                [ HH.text "A PCA projection tool" ]
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
        , HH.footer [ HP.classes [ HH.ClassName "footer" ] ]
            [ HH.div [ HP.classes [ HH.ClassName "content", HH.ClassName "has-text-centered" ] ]
                [ HH.p_
                    [ HH.strong_ [ HH.text "Authors: " ]
                    , HH.text "Stephan Schiffels ("
                    , externalLink "https://www.github.com/stschiff" "github.com/stschiff"
                    , HH.text "), Joscha Gretzinger"
                    ]
                , HH.p_
                    [ HH.strong_ [ HH.text "References: " ], HH.br_
                    , HH.text "Reference data: "
                    , externalLink "https://doi.org/10.1038/s41586-022-05247-2" "doi:10.1038/s41586-022-05247-2"
                    , HH.br_
                    , HH.text "Example data: "
                    , externalLink "https://doi.org/10.1038/s41562-024-01888-7" "doi:10.1038/s41562-024-01888-7"
                    ]
                ]
            ]
        ]

refDataBox :: forall slots m . (MonadAff m) => State -> H.ComponentHTML Action slots m
refDataBox st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Reference Data" ]
        , case st.refBundle of
            NotAsked -> HH.div_ [ HH.text "No reference data yet", HH.br_ ]
            Loading -> HH.div [ HP.classes [ HH.ClassName "is-flex", HH.ClassName "is-align-items-center" ] ]
                [ HH.span
                    [ HP.classes [ HH.ClassName "loader" ]
                    , HP.attr (HH.AttrName "style") "width: 1.2em; height: 1.2em; margin-right: 0.5em;"
                    ]
                    []
                , HH.text "Loading reference data\x2026"
                ]
            Failure err -> HH.div_ [ HH.text $ "Error loading refernece bundle: " <> err, HH.br_ ]
            Success rb -> HH.div_
                [ HH.text $ "Selected reference data with " <>
                    show rb.snpWeights.numSNPs <> " SNPs for " <>
                    show rb.snpWeights.numPCs <> " PCs and " <>
                    show rb.refPosData.numSamples <> " individuals", HH.br_
                , HH.button
                    [ HP.classes [ HH.ClassName "button", HH.ClassName "is-small", HH.ClassName "mt-2" ]
                    , HE.onClick (\_ -> DownloadRefPositions)
                    ]
                    [ HH.text "Download reference PC1/PC2 positions (CSV)" ]
                ]
        ]

projectionMonitor :: forall slots m . (MonadAff m) => State -> H.ComponentHTML Action slots m
projectionMonitor st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.div [ HP.classes [ HH.ClassName "box-header" ] ]
            [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
                [ HH.text "Projection Monitor" ]
            , HH.details [ HP.classes [ HH.ClassName "help-toggle" ] ]
                [ HH.summary_ [ HH.text "?" ]
                , HH.p_
                    [ HH.text
                        "Your loaded SNPs are matched against the reference panel's \
                        \SNPs by chromosome position. \"Included SNPs\" is the overlap \
                        \actually used for projection. \"Strand ambiguous\" SNPs (A/T or \
                        \C/G) are dropped because their strand can't be resolved from \
                        \alleles alone. \"Inconsistent\" SNPs have alleles that don't \
                        \match either orientation of the reference and are dropped too. \
                        \\"Flipped alleles\" counts SNPs read on the opposite strand from \
                        \the reference, which are automatically corrected rather than \
                        \dropped."
                    ]
                ]
            ]
        , case st.projectionResults of
            NotAsked -> HH.text "No projection performed yet"
            Loading -> HH.div [ HP.classes [ HH.ClassName "is-flex", HH.ClassName "is-align-items-center" ] ]
                [ HH.span
                    [ HP.classes [ HH.ClassName "loader" ]
                    , HP.attr (HH.AttrName "style") "width: 1.2em; height: 1.2em; margin-right: 0.5em;"
                    ]
                    []
                , HH.text "Projecting\x2026"
                ]
            Failure err -> HH.text $ "Error during projection: " <> err
            Success pr -> HH.div_
                [ HH.text $ "Number of samples projected: " <> show (length pr.projectionResults), HH.br_
                , HH.text $ "Included SNPs: " <> show pr.overlapReport.nrIncluded, HH.br_
                , HH.text $ "Strand Ambiguous Removed: " <> show pr.overlapReport.removedStrandAmbiguous, HH.br_
                , HH.text $ "Inconsistent Removed: " <> show pr.overlapReport.removedInconsistent, HH.br_
                , HH.text $ "Flipped alleles: " <> show pr.overlapReport.nrToBeFlipped
                ]
        ]

refChartBox :: forall action m . (MonadAff m) => State -> H.ComponentHTML action Slots m
refChartBox st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Reference Data Chart" ]
        , case st.refBundle of
            Success rb -> HH.div_
                [ HH.slot_ _refChart unit RefChart.component { refPosData: rb.refPosData, xPCindex: rb.pcaParams.defaultX,
                                                               yPCindex: rb.pcaParams.defaultY } ]
            _ -> HH.text ""
        ]

projChartBox :: forall m . (MonadAff m) => State -> H.ComponentHTML Action Slots m
projChartBox st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "Projection Results Chart" ]
        , case st.refBundle /\ st.userData /\ st.projectionResults of
            Success rb /\ Just pd /\ Success pr -> HH.div_
                [ HH.slot_ _projChart unit ProjChart.component
                    { refPosData: rb.refPosData
                    , projectedSamples: toProjectedSamples pd pr.projectionResults
                    , xPCindex: rb.pcaParams.defaultX
                    , yPCindex: rb.pcaParams.defaultY
                    }
                , HH.button
                    [ HP.classes [ HH.ClassName "button", HH.ClassName "is-small", HH.ClassName "mt-2" ]
                    , HE.onClick (\_ -> DownloadProjectedPositions)
                    ]
                    [ HH.text "Download projected PC1/PC2 positions (CSV)" ]
                ]
            _ -> HH.text "Projection results chart will be displayed here after running the projection."
        ]

toProjectedSamples :: PlinkData -> Array ProjectionResult -> Array ProjChart.ProjectedSample
toProjectedSamples pd results =
    zipWith (\(Tuple sampleID popGroup) pr -> { sampleID, popGroup, pcValues: pr.pcCoordinates, nrSNPs: pr.nonMissingCount })
        (zipWith Tuple pd.famData.indNames pd.famData.popNames)
        results

-- Escapes a field for CSV: wraps it in quotes (doubling any embedded quotes)
-- whenever it contains a comma, quote, or newline.
csvField :: String -> String
csvField s =
    if contains "," || contains "\"" || contains "\n"
        then "\"" <> replaceAll (Pattern "\"") (Replacement "\"\"") s <> "\""
        else s
    where
    contains pat = s /= replaceAll (Pattern pat) (Replacement "") s

pcAt :: Int -> Array Number -> String
pcAt idx pcValues = fromMaybe "" (show <$> (pcValues !! idx))

refPositionsCSV :: RefPosData -> String
refPositionsCSV rd =
    joinWith "\n" $ [ "sampleID,population,group,PC1,PC2" ] <>
        map row rd.samples
    where
    row s = joinWith ","
        [ csvField s.sampleID, csvField s.popName, csvField s.popGroup
        , pcAt 0 s.pcValues, pcAt 1 s.pcValues
        ]

projectedPositionsCSV :: Array ProjChart.ProjectedSample -> String
projectedPositionsCSV samples =
    joinWith "\n" $ [ "sampleID,group,PC1,PC2" ] <>
        map row samples
    where
    row s = joinWith ","
        [ csvField s.sampleID, csvField s.popGroup
        , pcAt 0 s.pcValues, pcAt 1 s.pcValues
        ]

initialState :: forall input. input -> State
initialState = const
    { refBundle : NotAsked
    , projectionResults : NotAsked
    , userData : Nothing
    }

selectedPanelKey :: String
selectedPanelKey = "WestEurasia_HiRes"

handleAction :: forall output slots m. MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction LoadRefData = do
    H.modify_ _ { refBundle = Loading }
    case Map.lookup selectedPanelKey panels of
        Nothing -> H.modify_ _ { refBundle = Failure $ "Unknown reference panel: " <> selectedPanelKey }
        Just panel -> do
            result <- H.liftAff $ attempt (loadReferenceBundle defaultFetcher panel)
            case result of
                Left err -> H.modify_ _ { refBundle = Failure (message err) }
                Right rb -> do
                    H.modify_ _ { refBundle = Success rb }
                    handleAction RunProjection

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

handleAction DownloadRefPositions = do
    st <- H.get
    case st.refBundle of
        Success rb -> liftEffect $ downloadCSV "reference_positions.csv" (refPositionsCSV rb.refPosData)
        _ -> pure unit

handleAction DownloadProjectedPositions = do
    st <- H.get
    case st.userData /\ st.projectionResults of
        Just pd /\ Success pr ->
            liftEffect $ downloadCSV "projected_positions.csv"
                (projectedPositionsCSV (toProjectedSamples pd pr.projectionResults))
        _ -> pure unit

-- This is just a hack for now, to make sure the spinner starts running while the synchronous projection computation runs.
nextAnimationFrame :: forall m. MonadAff m => m Unit
nextAnimationFrame = liftAff $ makeAff \callback -> do
  win <- window
  _ <- requestAnimationFrame (callback (Right unit)) win
  pure nonCanceler
