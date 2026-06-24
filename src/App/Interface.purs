module App.Interface where

import Prelude

import App.RefChart as RefChart
import PCproject.PCproject (ProjectionResult, projectPlinkOnWeights)
import PCproject.PlinkData (PlinkData, readBimData, readFamData, checkBedFileMagicBytes)
import PCproject.Plot (drawChart)
import PCproject.RefPosData (RefPosData, readRefPosData)
import PCproject.SnpWeights (SnpWeights, readSnpWeights)

import Data.Array (filter, length)
import Data.ArrayBuffer.Typed (whole)
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect, class MonadEffect)
-- import Effect.Class.Console (log)
import Effect.Exception (error)
import Fetch (fetch)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel
import Web.Event.Event as WE
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File (File, name, toBlob)
import Web.File.FileList (items)
import Web.File.FileReader (fileReader, readAsArrayBuffer, result, toEventTarget)
import Web.HTML.Event.EventTypes (load, error) as ET
import Web.HTML.HTMLInputElement (fromEventTarget, files)

-- Function to read file contents using FileReader API with makeAff
readFileAsArrayBufferAff :: forall m. MonadAff m => File -> m ArrayBuffer
readFileAsArrayBufferAff file = liftAff $ makeAff \callback -> do
  reader <- fileReader
  
  -- Set up success handler
  loadListener <- eventListener \_ -> do
    foreignResult <- result reader
    callback (Right $ unsafeFromForeign foreignResult)
  
  -- Set up error handler  
  errorListener <- eventListener \_ -> do
    callback (Left (error "FileReader error"))
  
  -- Add event listeners to the reader (converted to EventTarget)
  let eventTarget = toEventTarget reader
  addEventListener ET.load loadListener false eventTarget
  addEventListener ET.error errorListener false eventTarget
  
  -- Start reading (convert File to Blob first)
  readAsArrayBuffer (toBlob file) reader
  
  pure nonCanceler

arrayBufferToString :: forall m. MonadEffect m => ArrayBuffer -> m String
arrayBufferToString buffer = liftEffect $ do
  decoder <- TextDecoder.new UtfLabel.utf8
  arrayView <- whole buffer :: Effect (Uint8Array)
  TextDecoder.decode arrayView decoder

data PlinkFileSpec = PlinkFileSpec File File File

type State =
  { selectedPlinkFiles :: Maybe PlinkFileSpec
  , statusPlinkFilesLoading :: Boolean
  , snpWeights :: Maybe SnpWeights
  , plinkData :: Maybe PlinkData
  , refData :: Maybe RefPosData
  , projectionRunning :: Boolean
  , projectionResults :: Maybe ProjectionResult
  , errorNote :: Maybe String
  }

data Action
  = LoadRefData
  | GotGenoDataFileEvent WE.Event
  | RunProjectionEvent
  | MakeChart

type Slots = ( refChart :: forall q o . H.Slot q o Unit )

_refChart = Proxy :: Proxy "refChart"

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
    [ HH.h1 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-1"] ] [ HH.text "PC Projection Tool" ]
    , HH.div [ HP.classes [ HH.ClassName "columns" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "column" ] ] [ refDataBox st ]
      , HH.div [ HP.classes [ HH.ClassName "column" ] ] [ userDataBox st ]
      ]
    , HH.div [ HP.classes [ HH.ClassName "columns" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "column" ] ] [ refChartBox st ]
      , HH.div [ HP.classes [ HH.ClassName "column" ] ] [ projChartBox st ]
      ]
    ]

refDataBox :: forall action slots m . (MonadAff m) => State -> H.ComponentHTML action slots m
refDataBox st = 
  HH.div [ HP.classes [ HH.ClassName "box" ] ]
    [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ] [ HH.text "Reference Data" ]
    , case st.snpWeights of
        Nothing -> HH.div_ [ HH.text "Loading weight file...", HH.br_ ]
        Just sw -> HH.div_ [ HH.text $ "Selected weight file with SNPs: " <> show sw.numSNPs <> ", PCs: " <> show sw.numPCs, HH.br_ ]
    , case st.refData of
        Nothing -> HH.div_ [ HH.text "Loading reference position file...", HH.br_ ]
        Just rd -> HH.div_ [ HH.text $ "Reference Position Data loaded. Samples: " <> show rd.numSamples <> ", PCs: " <> show rd.numPCs, HH.br_ ]
    ]

userDataBox :: forall slots m . (MonadAff m) => State -> H.ComponentHTML Action slots m
userDataBox st =
  HH.div [ HP.classes [ HH.ClassName "box" ] ]
    [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ] [ HH.text "User Data" ]
    , HH.label_ [ HH.text "Select Plink genotype data files: " ]
    , fileInputForm
          , case st.selectedPlinkFiles of
      Nothing -> HH.div_ [ HH.text "No plink files selected", HH.br_ ]
      Just (PlinkFileSpec famFile bimFile bedFile) -> HH.div_
        [ HH.text $ "Selected fam file: " <> name famFile
        , HH.br_
        , HH.text $ "Selected bim file: " <> name bimFile
        , HH.br_
        , HH.text $ "Selected bed file: " <> name bedFile
        ]
    , if st.statusPlinkFilesLoading then
        HH.div_ [ HH.text "Loading plink files...", HH.br_ ]
      else
        HH.text ""
    , case st.plinkData of
        Nothing -> HH.text ""
        Just pd -> HH.div_ [ HH.text $ "Plink Data loaded. Individuals: " <> show pd.numIndividuals <> ", SNPs: " <> show pd.numSNPs, HH.br_ ]
    , if isJust st.snpWeights && isJust st.plinkData && (not st.projectionRunning) && (isNothing st.projectionResults) then
        HH.button [ HE.onClick (\_ -> RunProjectionEvent) ] [ HH.text "Run Projection" ]
      else
        HH.text ""
    , if st.projectionRunning then
        HH.div_ [ HH.text "Projection running...", HH.br_ ]
      else
        HH.text ""
    , case st.projectionResults of
        Nothing -> HH.text ""
        Just pr -> HH.div_ [ HH.text $ "Projection done. Overlapping SNPs: " <> show pr.overlappingPositions <> ", PCs: " <> show pr.numPCs <> ", Individuals: " <> show pr.numIndividuals, HH.br_ ]
    , case st.errorNote of
        Nothing -> HH.text ""
        Just errMsg -> HH.div_ [ HH.text $ "Error: " <> errMsg, HH.br_ ]
    , if isJust st.refData || isJust st.projectionResults then
        HH.button [ HE.onClick (\_ -> MakeChart) ] [ HH.text "Make Chart" ]
      else
        HH.text ""
    ]

refChartBox :: forall action m . (MonadAff m) => State -> H.ComponentHTML action Slots m
refChartBox st =
  HH.div [ HP.classes [ HH.ClassName "box" ] ]
    [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ] [ HH.text "Reference Data Chart" ]
    , case st.refData of
        Nothing -> HH.text ""
        Just rd -> HH.div_ [ HH.slot_ _refChart unit RefChart.component { refPosData:  rd } ]
    ]

projChartBox :: forall action slots m . (MonadAff m) => State -> H.ComponentHTML action slots m
projChartBox _ =
  HH.div [ HP.classes [ HH.ClassName "box" ] ]
    [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ] [ HH.text "Projection Results Chart" ]
    , HH.text "Projection results chart will be displayed here after running the projection."
    ]

fileInputForm :: forall slots m . H.ComponentHTML Action slots m
fileInputForm = HH.form_
    [ HH.label_ [ HH.text "Select Plink genotype data files: " ]
    , HH.input  
      [ HP.type_ HP.InputFile, HP.multiple true, HE.onChange GotGenoDataFileEvent ]
    , HH.br_
    ]

initialState :: forall input. input -> State
initialState = const
  { selectedPlinkFiles: Nothing
  , statusPlinkFilesLoading : false
  , snpWeights : Nothing
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

handleAction (GotGenoDataFileEvent ev) = do
  let mInputElem = WE.target ev >>= fromEventTarget
  case mInputElem of
    Nothing -> pure unit
    Just inputElem -> do
      mFileList <- liftEffect $ files inputElem
      case mFileList of
        Nothing -> pure unit
        Just fileList -> do
          let files = items fileList
          case filter (\f -> endsWith ".fam" (name f)) files of
            [] -> H.modify_ _ { errorNote = Just "No .fam file selected", selectedPlinkFiles = Nothing }
            [famFile] -> case filter (\f -> endsWith ".bim" (name f)) files of
              [] -> H.modify_ _ { errorNote = Just "No .bim file selected", selectedPlinkFiles = Nothing }
              [bimFile] -> case filter (\f -> endsWith ".bed" (name f)) files of
                [] -> H.modify_ _ { errorNote = Just "No .bed file selected", selectedPlinkFiles = Nothing }
                [bedFile] -> do
                  H.modify_ _ { selectedPlinkFiles = Just (PlinkFileSpec famFile bimFile bedFile), errorNote = Nothing }
                  H.modify_ _ { statusPlinkFilesLoading = true }
                  -- Read the files asynchronously using makeAff
                  famContent <- readFileAsArrayBufferAff famFile >>= arrayBufferToString
                  bimContent <- readFileAsArrayBufferAff bimFile >>= arrayBufferToString
                  bedContent <- readFileAsArrayBufferAff bedFile
                  let famResults = readFamData famContent
                  let bimResults = readBimData bimContent
                  bedCheck <- liftEffect $ checkBedFileMagicBytes bedContent
                  if (not bedCheck) then
                    H.modify_ _ { errorNote = Just "Invalid .bed file (incorrect magic numbers in the first three bytes)", selectedPlinkFiles = Nothing, statusPlinkFilesLoading = false }
                  else do
                    let plinkData = { famData : famResults, bimData : bimResults, bedData : bedContent, numIndividuals : length famResults.indNames, numSNPs : length bimResults.snpIDs }
                    H.modify_ _ { plinkData = Just plinkData, statusPlinkFilesLoading = false } 
                _ -> H.modify_ _ { errorNote = Just "Multiple .bed files selected", selectedPlinkFiles = Nothing }
              _ -> H.modify_ _ { errorNote = Just "Multiple .bim files selected", selectedPlinkFiles = Nothing }
            _ -> H.modify_ _ { errorNote = Just "Multiple .fam files selected", selectedPlinkFiles = Nothing }
  pure unit

handleAction RunProjectionEvent = do
  H.modify_ _ { projectionRunning = true, projectionResults = Nothing }
  checkAndRunProjection
  H.modify_ _ { projectionRunning = false }
  pure unit

handleAction MakeChart = do
  st <- H.get
  case st.refData of
    Nothing -> pure unit
    Just rd ->
      liftEffect $ drawChart rd 0 1

checkAndRunProjection :: forall output slots m. MonadAff m => H.HalogenM State Action slots output m Unit
checkAndRunProjection = do
  st <- H.get
  case Tuple st.snpWeights st.plinkData of
    Tuple (Just sw) (Just pd) -> do
      let projResult = projectPlinkOnWeights pd sw
      H.modify_ _ { projectionResults = Just projResult }
    _ -> pure unit
  pure unit

