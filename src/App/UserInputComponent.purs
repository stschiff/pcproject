module App.UserInputComponent where

import Prelude

import Data.Array (filter, length)
import Data.ArrayBuffer.Typed (whole)
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Utils (endsWith)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Exception (error)
import Fetch (fetch)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PCproject.PlinkData (PlinkData, readBimData, readFamData, checkBedFileMagicBytes)
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel
import Web.Event.Event as WE
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File (File, name, toBlob)
import Web.File.FileList (items)
import Web.File.FileReader (fileReader, readAsArrayBuffer, result, toEventTarget)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLInputElement (fromEventTarget, files)

data PlinkFileSpec = PlinkFileSpec File File File | ExampleData

type State =
  { selectedPlinkFiles :: Maybe PlinkFileSpec
  , statusPlinkFilesLoading :: Boolean
  , errorNote :: Maybe String
  }

type Output = PlinkData

data Action
    = GotGenoDataFileEvent WE.Event
    | RequestSampleData


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


component :: forall query i m. MonadAff m => H.Component query i Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

initialState :: forall i. i -> State
initialState _ =
    { selectedPlinkFiles: Nothing
    , statusPlinkFilesLoading: false
    , errorNote: Nothing }

render :: forall slots m . (MonadAff m) => State -> H.ComponentHTML Action slots m
render st =
    HH.div [ HP.classes [ HH.ClassName "box" ] ]
        [ HH.h2 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
            [ HH.text "User Data" ]
        , HH.div [ HP.classes [ HH.ClassName "field" ] ]
            [ HH.label_ [ HH.text "Select Plink genotype data files: " ]
            , HH.div [ HP.classes [ HH.ClassName "control" ] ]
                [ HH.input
                    [ HP.type_ HP.InputFile, HP.multiple true, HE.onChange GotGenoDataFileEvent ]
                ]
            ]
        , case st.selectedPlinkFiles of
            Nothing -> HH.div_ [ HH.text "No plink files selected", HH.br_ ]
            Just (PlinkFileSpec famFile bimFile bedFile) -> HH.div_
                [ HH.text $ "Selected fam file: " <> name famFile
                , HH.br_
                , HH.text $ "Selected bim file: " <> name bimFile
                , HH.br_
                , HH.text $ "Selected bed file: " <> name bedFile
                ]
            Just ExampleData -> HH.div_ [ HH.text "Using example data", HH.br_ ]
        , HH.div [ HP.classes [ HH.ClassName "control" ] ]
            [ HH.button
                [ HP.classes [ HH.ClassName "button", HH.ClassName "is-primary" ]
                , HE.onClick (\_ -> RequestSampleData)
                ]
                [ HH.text "Load Example Data" ]
            ]
        , if st.statusPlinkFilesLoading then
            HH.div_ [ HH.text "Loading plink files...", HH.br_ ]
        else
            HH.text ""
        ]

handleAction :: forall slots m. MonadAff m => Action -> H.HalogenM State Action slots Output m Unit
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
                    H.modify_ _ { statusPlinkFilesLoading = false }
                    H.raise plinkData
                _ -> H.modify_ _ { errorNote = Just "Multiple .bed files selected", selectedPlinkFiles = Nothing }
              _ -> H.modify_ _ { errorNote = Just "Multiple .bim files selected", selectedPlinkFiles = Nothing }
            _ -> H.modify_ _ { errorNote = Just "Multiple .fam files selected", selectedPlinkFiles = Nothing }

handleAction RequestSampleData = do
    famFetch <- H.liftAff $ fetch "./assets/2024_Gretzinger_EarlyCelts_shortened.fam" {}
    bimFetch <- H.liftAff $ fetch "./assets/2024_Gretzinger_EarlyCelts_shortened.bim" {}
    bedFetch <- H.liftAff $ fetch "./assets/2024_Gretzinger_EarlyCelts_shortened.bed" {}
    if famFetch.ok && bimFetch.ok && bedFetch.ok
        then do
            famContent <- H.liftAff $ famFetch.text
            bimContent <- H.liftAff $ bimFetch.text
            bedContent <- H.liftAff $ bedFetch.arrayBuffer
            let famResults = readFamData famContent
            let bimResults = readBimData bimContent
            H.raise
                { famData : famResults
                , bimData : bimResults
                , bedData : bedContent
                , numIndividuals : length famResults.indNames
                , numSNPs : length bimResults.snpIDs
                }
        else
            H.modify_ _ { errorNote = Just "Failed to load example data" }
