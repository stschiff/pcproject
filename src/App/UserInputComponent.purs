module App.UserInputComponent where

import Prelude

import App.Utils (RemoteData(..))
import Data.Array (filter, length, concat)
import Data.ArrayBuffer.Typed (whole)
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Exception (error)
import Fetch (fetch)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PCproject.PlinkData (PlinkData, readBimData, readFamData, readBedData)
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel
import Web.Event.Event as WE
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File (File, name, toBlob)
import Web.File.FileList (items)
import Web.File.FileReader (fileReader, readAsArrayBuffer, result, toEventTarget)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLInputElement (fromEventTarget, files)

data State =
  NoData
  | FromUserUpload (RemoteData String PlinkData)
  | FromExampleData (RemoteData String PlinkData)

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
initialState _ = NoData

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
        , let classes = concat [ [ HH.ClassName "button", HH.ClassName "is-primary" ], 
                case st of
                  FromExampleData (Loading) -> [ HH.ClassName "is-loading" ]
                  _ -> [] ]
          in 
            HH.div [ HP.classes [ HH.ClassName "control" ] ]
              [ HH.button
                [ HP.classes classes, HE.onClick (\_ -> RequestSampleData) ]
                [ HH.text "Load Example Data" ]
              ]
        , case st of
            NoData -> HH.div_ [ HH.text "No data selected", HH.br_ ]
            FromUserUpload pd -> HH.div_
              [ HH.text "Using user-uploaded data"
              , case pd of
                  NotAsked -> HH.text "No data loaded yet"
                  Loading -> HH.text "Loading data from files..."
                  Failure err -> HH.text $ "Error loading data from files: " <> err
                  Success plinkData -> HH.text $ "Loaded data with " <>
                    show plinkData.numIndividuals <> " individuals and " <>
                    show plinkData.numSNPs <> " SNPs"
              , HH.br_
              ]
            FromExampleData pd -> HH.div_
                [ HH.text "Using example data"
                , case pd of
                    NotAsked -> HH.text "No data loaded yet"
                    Loading -> HH.text "Loading example data..."
                    Failure err -> HH.text $ "Error loading example data: " <> err
                    Success plinkData -> HH.text $ "Loaded example data with " <>
                      show plinkData.numIndividuals <> " individuals and " <> show plinkData.numSNPs <> " SNPs"
                , HH.br_
                ]
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
            [] -> H.modify_ (\_ -> FromUserUpload (Failure "No .fam file given"))
            [famFile] -> case filter (\f -> endsWith ".bim" (name f)) files of
              [] -> H.modify_ (\_ -> FromUserUpload (Failure "No .bim file selected"))
              [bimFile] -> case filter (\f -> endsWith ".bed" (name f)) files of
                [] -> H.modify_ (\_ -> FromUserUpload (Failure "No .bed file selected"))
                [bedFile] -> do
                  H.modify_ (\_ -> FromUserUpload Loading)
                  -- Read the files asynchronously using makeAff
                  famResult <- liftAff $ attempt (readFileAsArrayBufferAff famFile >>= arrayBufferToString >>= readFamData)
                  bimResult <- liftAff $ attempt (readFileAsArrayBufferAff bimFile >>= arrayBufferToString >>= readBimData)
                  case Tuple famResult bimResult of
                    Tuple (Right fam) (Right bim) -> do
                      let numInds = length fam.indNames
                      let numSNPs = length bim.snpIDs
                      bedResult <- liftAff $ attempt (readFileAsArrayBufferAff bedFile >>= \bedContent -> readBedData bedContent numSNPs numInds)
                      case bedResult of
                        Left err -> H.modify_ (\_ -> FromUserUpload (Failure $ "Error reading .bed file: " <> show err))
                        Right bed -> do
                          let plinkData = { famData : fam, bimData : bim, bedData : bed, numIndividuals : numInds, numSNPs : numSNPs }
                          H.modify_ (\_ -> FromUserUpload (Success plinkData))
                          H.raise plinkData
                    Tuple (Left err) _ -> H.modify_ (\_ -> FromUserUpload (Failure $ "Error: " <> show err))
                    Tuple _ (Left err) -> H.modify_ (\_ -> FromUserUpload (Failure $ "Error: " <> show err))
                _ -> H.modify_ (\_ -> FromUserUpload (Failure "Multiple .bed files selected"))
              _ -> H.modify_ (\_ -> FromUserUpload (Failure "Multiple .bim files selected"))
            _ -> H.modify_ (\_ -> FromUserUpload (Failure "Multiple .fam files selected"))

handleAction RequestSampleData = do
    H.modify_ (\_ -> FromExampleData Loading)
    famFetch <- H.liftAff $ fetch "./assets/2024_Gretzinger_EarlyCelts.fam" {}
    bimFetch <- H.liftAff $ fetch "./assets/2024_Gretzinger_EarlyCelts.bim" {}
    bedFetch <- H.liftAff $ fetch "./assets/2024_Gretzinger_EarlyCelts.bed" {}
    if famFetch.ok && bimFetch.ok && bedFetch.ok
        then do
            famResult <- H.liftAff $ attempt (famFetch.text >>= readFamData)
            bimResult <- H.liftAff $ attempt (bimFetch.text >>= readBimData)
            case Tuple famResult bimResult of
                Tuple (Right fam) (Right bim) -> do
                    let numInds = length fam.indNames
                    let numSNPs = length bim.snpIDs
                    bedResult <- H.liftAff $ attempt (bedFetch.arrayBuffer >>= \bedContent -> readBedData bedContent numSNPs numInds)
                    case bedResult of
                        Left err -> H.modify_ (\_ -> FromExampleData (Failure $ "Error reading .bed file: " <> show err))
                        Right bed -> do
                            let plinkData = { famData : fam, bimData : bim, bedData : bed, numIndividuals : numInds, numSNPs : numSNPs }
                            H.modify_ (\_ -> FromExampleData (Success plinkData))
                            H.raise plinkData
                Tuple (Left err) _ -> H.modify_ (\_ -> FromExampleData (Failure $ "Error: " <> show err))
                Tuple _ (Left err) -> H.modify_ (\_ -> FromExampleData (Failure $ "Error: " <> show err))
        else
            H.modify_ (\_ -> FromExampleData (Failure "Failed to load example data"))