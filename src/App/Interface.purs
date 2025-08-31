module App.Interface where


import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Foreign (readString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PCproject.PlinkData (PlinkData, readBimData, readFamData)
import PCproject.SnpWeights (SnpWeights, readSnpWeights)
import Web.Event.Event as WE
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File (File, name, size, toBlob)
import Web.File.FileList (item, items)
import Web.File.FileReader (fileReader, readAsText, result, toEventTarget)
import Web.HTML.Event.EventTypes (load, error) as ET
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLInputElement (fromEventTarget, files)

-- Function to read file contents using FileReader API with makeAff
readFileAsTextAff :: forall state action slots output m. MonadAff m => File -> H.HalogenM state action slots output m String
readFileAsTextAff file = liftAff $ makeAff \callback -> do
  reader <- fileReader
  
  -- Set up success handler
  loadListener <- eventListener \_ -> do
    foreignResult <- result reader
    content <- runExceptT $ readString foreignResult
    case content of
      Left (NonEmptyList foreignErrs) -> callback (Left $ error (show foreignErrs))
      Right contentText -> callback (Right contentText)
  
  -- Set up error handler  
  errorListener <- eventListener \_ -> do
    callback (Left (error "FileReader error"))
  
  -- Add event listeners to the reader (converted to EventTarget)
  let eventTarget = toEventTarget reader
  addEventListener ET.load loadListener false eventTarget
  addEventListener ET.error errorListener false eventTarget
  
  -- Start reading (convert File to Blob first)
  readAsText (toBlob file) reader
  
  pure nonCanceler

data PlinkFileSpec = PlinkFileSpec File File File

type State =
  { selectedWeightFile :: Maybe File
  , selectedPlinkFiles :: Maybe PlinkFileSpec
  , statusWeightFileLoading :: Boolean
  , statusPlinkFilesLoading :: Boolean
  , snpWeights :: Maybe SnpWeights
  , plinkData :: Maybe PlinkData
  , errorNote :: Maybe String
  }

data Action
  = GotWeightFileEvent WE.Event
  | GotGenoDataFileEvent WE.Event

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState = const
  { selectedWeightFile: Nothing
  , selectedPlinkFiles: []
  , statusWeightFileLoading : false
  , statusPlinkFilesLoading : false
  , snpWeights : Nothing
  , plinkData : Nothing
  , errorNote : Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div_
    [ HH.h3_ [ HH.text "Upload files" ]
    , fileInputForm
    , case st.selectedWeightFile of
        Nothing -> HH.text "No weight file selected"
        Just file -> HH.div_
          [ HH.text $ "Selected weight file: " <> name file
          , HH.br_
          , HH.text $ "Weight File size: " <> show (size file) <> " bytes"
          ]
    , case st.selectedPlinkFiles of
        Nothing -> HH.text "No plink files selected"
        Just (PlinkFileSpec famFile bimFile bedFile) -> HH.div_
          [ HH.text $ "Selected fam file: " <> name famFile
          , HH.br_
          , HH.text $ "Selected bim file: " <> name bimFile
          , HH.br_
          , HH.text $ "Selected bed file: " <> name bedFile
          , HH.br_
          , HH.text $ "Bed File size: " <> show (size bedFile) <> " bytes"
          , HH.br_
          , HH.text $ "Bim File size: " <> show (size bimFile) <> " bytes"
          ]
    , if st.statusWeightFileLoading then
        HH.div_ [ HH.text "Loading weight file...", HH.br_ ]
      else
        HH.text ""
    , if st.statusPlinkFilesLoading then
        HH.div_ [ HH.text "Loading plink files...", HH.br_ ]
      else
        HH.text ""
    , case st.snpWeights of
        Nothing -> HH.text ""
        Just _ -> HH.text $ "snpWeights loaded" -- "SNPs: " <> show sw.numSnps <> ", PCs: " <> show sw.numPCs
    , case st.plinkData of
        Nothing -> HH.text ""
        Just _ -> HH.text $ "Plink Data loaded"
    , case st.errorNote of
        Nothing -> HH.text ""
        Just errMsg -> HH.div_ [ HH.text $ "Error: " <> errMsg, HH.br_ ]
    ]
  where
    fileInputForm :: H.ComponentHTML Action () m
    fileInputForm = HH.form_
        [ HH.label_ [ HH.text "Select a weight file: " ]
        , HH.input 
          [ HP.type_ HP.InputFile, HE.onChange GotWeightFileEvent ]
        , HH.br_
        , HH.label_ [ HH.text "Select Plink genotype data files: " ]
        , HH.input  
          [ HP.type_ HP.InputFile, HP.multiple true, HE.onChange GotGenoDataFileEvent ]
        ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction (GotWeightFileEvent ev) = do
  let mInputElem = WE.target ev >>= fromEventTarget
  mFile <- case mInputElem of
    Nothing -> pure Nothing
    Just inputElem -> do
      mFileList <- liftEffect $ files inputElem
      pure $ mFileList >>= item 0
  case mFile of
    Just file -> do
      H.modify_ _ { selectedWeightFile = Just file }
      H.modify_ _ { statusWeightFileLoading = true }
      -- Read file contents asynchronously using makeAff
      content <- readFileAsTextAff file
      let snpWeightData = readSnpWeights content
      H.modify_ _ { snpWeights = Just snpWeightData, statusWeightFileLoading = false }
    Nothing -> pure unit

handleAction (GotGenoDataFileEvent ev) = do
  let mInputElem = WE.target ev >>= fromEventTarget
  case mInputElem of
    Nothing -> pure Nothing
    Just inputElem -> do
      mFileList <- liftEffect $ files inputElem
      case mFileList of
        Nothing -> pure Nothing
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
                  famContent <- readFileAsTextAff famFile
                  bimContent <- readFileAsTextAff bimFile
                  ...
                _ -> H.modify_ _ { errorNote = Just "Multiple .bed files selected", selectedPlinkFiles = Nothing }
              _ -> H.modify_ _ { errorNote = Just "Multiple .bim files selected", selectedPlinkFiles = Nothing }
            _ -> H.modify_ _ { errorNote = Just "Multiple .fam files selected", selectedPlinkFiles = Nothing }
  pure unit


