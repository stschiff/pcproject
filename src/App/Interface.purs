module App.Interface
  ( Action(..)
  , PlinkFileSpec(..)
  , Slots
  , State
  , _refChart
  , _userInputComponent
  , component
  , handleAction
  , initialState
  , projChartBox
  , refChartBox
  , refDataBox
  , render
  )
  where

import Prelude

import App.RefChart as RefChart
import App.UserInputComponent as UserInputComponent
import PCproject.PCproject (ProjectionResult, projectPlinkOnWeights)
import PCproject.PlinkData (PlinkData)
import PCproject.RefPosData (RefPosData, readRefPosData)
import PCproject.SnpWeights (SnpWeights, readSnpWeights)

import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
-- import Effect.Class.Console (log)
import Fetch (fetch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.File.File (File)

data PlinkFileSpec = PlinkFileSpec File File File

type State =
  { snpWeights :: Maybe SnpWeights
  , plinkData :: Maybe PlinkData
  , refData :: Maybe RefPosData
  , projectionRunning :: Boolean
  , projectionResults :: Maybe ProjectionResult
  , errorNote :: Maybe String
  }

data Action
  = LoadRefData
  | GotPlinkData PlinkData
  | RunProjectionEvent

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
            , HH.div [ HP.classes [ HH.ClassName "column" ] ]
                [ HH.slot _userInputComponent unit UserInputComponent.component unit GotPlinkData ]
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
            [ HH.text "Data Monitor" ]
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
            , case st.plinkData of
                Nothing -> HH.text ""
                Just pd -> HH.div_
                    [ HH.text $ "Plink Data loaded. Individuals: " <>
                        show pd.numIndividuals <> ", SNPs: " <> show pd.numSNPs, HH.br_
                    ]
        , if isJust st.snpWeights && isJust st.plinkData &&
            (not st.projectionRunning) && (isNothing st.projectionResults) then
            HH.div [ HP.classes [ HH.ClassName "control" ] ]
                [ HH.button
                    [ HP.classes [ HH.ClassName "button", HH.ClassName "is-primary" ]
                    , HE.onClick (\_ -> RunProjectionEvent)
                    ]
                    [ HH.text "Run Projection" ]
                ]
        else
            HH.text ""
        , if st.projectionRunning then
            HH.div_ [ HH.text "Projection running...", HH.br_ ]
        else
            HH.text ""
        , case st.projectionResults of
            Nothing -> HH.text ""
            Just pr -> HH.div_
                [ HH.text $ "Projection done. Overlapping SNPs: " <>
                    show pr.overlappingPositions <> ", PCs: " <>
                    show pr.numPCs <> ", Individuals: " <>
                    show pr.numIndividuals, HH.br_
                ]
        , case st.errorNote of
            Nothing -> HH.text ""
            Just errMsg -> HH.div_ [ HH.text $ "Error: " <> errMsg, HH.br_ ]
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
    , plinkData : Nothing
    , refData : Nothing
    , projectionRunning : false
    , projectionResults : Nothing
    , errorNote : Nothing
    }

handleAction :: forall output slots m. MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction LoadRefData = do
    f1 <- H.liftAff $ fetch "./assets/weights_joined.tsv" {}
    if f1.ok
        then do
            content <- H.liftAff $ f1.text
            let snpWeightData = readSnpWeights content
            H.modify_ _ { snpWeights = Just snpWeightData}
        else
            H.modify_ _ { errorNote = Just "Failed to load weight data", snpWeights = Nothing}
    f2 <- H.liftAff $ fetch "./assets/reference_positions.tsv" {}
    if f2.ok
        then do
            content <- H.liftAff $ f2.text
            let refData = readRefPosData content
            H.modify_ _ { refData = Just refData}
        else
            H.modify_ _ { errorNote = Just "Failed to load reference data", refData = Nothing}
    st <- H.get
    when (isJust st.plinkData) $
        handleAction RunProjectionEvent

handleAction (GotPlinkData pd) = do
    H.modify_ _ { plinkData = Just pd, errorNote = Nothing }
    st <- H.get
    when (isJust st.snpWeights && isJust st.refData) $
        handleAction RunProjectionEvent
 
handleAction RunProjectionEvent = do
    H.modify_ _ { projectionRunning = true, projectionResults = Nothing }
    overlapReport <- liftEffect $ overlapReport st.plinkData st.snpWeights
    H.modify_ _ { projectionRunning = false }
    pure unit
