module App.UserInputComponent where

import Prelude

import Data.Array (filter, length)
import Data.ArrayBuffer.Typed (whole)
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Utils (endsWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Exception (error, try)
import Fetch (fetch)
import Foreign (unsafeFromForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel
import Web.Event.Event as WE
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File (File, name, toBlob)
import Web.File.FileList (items)
import Web.File.FileReader (fileReader, readAsArrayBuffer, result, toEventTarget)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLInputElement (fromEventTarget, files)

import PCproject.PlinkData (PlinkData, readBimData, readFamData, readBedData)

data PlinkFileSpec = PlinkFileSpec File File File | ExampleData

type State =
  { selectedPlinkFiles :: Maybe PlinkFileSpec
  , errorNote :: Maybe String
  , plinkData :: Maybe PlinkData
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
    , errorNote: Nothing
    , plinkData: Nothing
    }

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
        , case Tuple st.plinkData st.selectedPlinkFiles of
            Tuple _ Nothing -> HH.text ""
            Tuple Nothing _ -> HH.div_ [ HH.text "Loading plink files...", HH.br_ ]
            Tuple (Just pd) _ ->
              HH.div_
                [ HH.text $ "Nr of samples to project: " <> show pd.numIndividuals, HH.br_
                , HH.text $ "Nr of SNPs: " <> show pd.numSNPs, HH.br_
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
            [] -> H.modify_ _ { errorNote = Just "No .fam file selected", selectedPlinkFiles = Nothing }
            [famFile] -> case filter (\f -> endsWith ".bim" (name f)) files of
              [] -> H.modify_ _ { errorNote = Just "No .bim file selected", selectedPlinkFiles = Nothing }
              [bimFile] -> case filter (\f -> endsWith ".bed" (name f)) files of
                [] -> H.modify_ _ { errorNote = Just "No .bed file selected", selectedPlinkFiles = Nothing }
                [bedFile] -> do
                  H.modify_ _ { selectedPlinkFiles = Just (PlinkFileSpec famFile bimFile bedFile), errorNote = Nothing }
                  -- Read the files asynchronously using makeAff
                  famResult <- liftAff $ attempt (readFileAsArrayBufferAff famFile >>= arrayBufferToString >>= readFamData)
                  bimResult <- liftAff $ attempt (readFileAsArrayBufferAff bimFile >>= arrayBufferToString >>= readBimData)
                  case Tuple famResult bimResult of
                    Tuple (Right fam) (Right bim) -> do
                      let numInds = length fam.indNames
                      let numSNPs = length bim.snpIDs
                      bedResult <- liftAff $ attempt (readFileAsArrayBufferAff bedFile >>= \bedContent -> readBedData bedContent numSNPs numInds)
                      case bedResult of
                        Left err -> H.modify_ _ { errorNote = Just $ "Error reading .bed file: " <> show err }
                        Right bed -> do
                          let plinkData = { famData : fam, bimData : bim, bedData : bed, numIndividuals : numInds, numSNPs : numSNPs }
                          H.modify_ _ { plinkData = Just plinkData }
                          H.raise plinkData
                    Tuple (Left err) _ -> H.modify_ _ { errorNote = Just $ "Error: " <> show err }
                    Tuple _ (Left err) -> H.modify_ _ { errorNote = Just $ "Error: " <> show err }
                _ -> H.modify_ _ { errorNote = Just "Multiple .bed files selected", selectedPlinkFiles = Nothing }
              _ -> H.modify_ _ { errorNote = Just "Multiple .bim files selected", selectedPlinkFiles = Nothing }
            _ -> H.modify_ _ { errorNote = Just "Multiple .fam files selected", selectedPlinkFiles = Nothing }

handleAction RequestSampleData = do
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
                        Left err -> H.modify_ _ { errorNote = Just $ "Error reading .bed file: " <> show err }
                        Right bed -> do
                            let plinkData = { famData : fam, bimData : bim, bedData : bed, numIndividuals : numInds, numSNPs : numSNPs }
                            H.modify_ _ { plinkData = Just plinkData, selectedPlinkFiles = Just ExampleData, errorNote = Nothing }
                            H.raise plinkData
                Tuple (Left err) _ -> H.modify_ _ { errorNote = Just $ "Error: " <> show err }
                Tuple _ (Left err) -> H.modify_ _ { errorNote = Just $ "Error: " <> show err }
        else
            H.modify_ _ { errorNote = Just "Failed to load example data" }
