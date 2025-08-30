module App.Interface where

import Prelude

import PCproject.SnpWeights (readSnpWeights)

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.ST.Internal (read)
import Data.Array (length)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Data.String.Common (trim)
import Data.Traversable (traverse)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (readString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File (File, name, size, toBlob)
import Web.File.FileList (item)
import Web.File.FileReader (fileReader, readAsText, result, toEventTarget)
import Web.HTML.Event.EventTypes (load, error) as ET
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

type State =
  { selectedWeightFile :: Maybe File
  , statusWeightFileLoading :: Boolean
  , nrSnps :: Maybe Int
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
  , statusWeightFileLoading : false
  , nrSnps : Nothing }

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
    , if st.statusWeightFileLoading then
        HH.div_ [ HH.text "Loading weight file...", HH.br_ ]
      else
        HH.text ""
    , case st.nrSnps of
        Nothing -> HH.text ""
        Just nr -> HH.text $ "SNPs: " <> show nr
    ]
  where
    fileInputForm :: H.ComponentHTML Action () m
    fileInputForm = HH.form_
        [ HH.label_ [ HH.text "Select a weight file: " ]
        , HH.input 
          [ HP.type_ HP.InputFile, HE.onChange GotWeightFileEvent ]
        , HH.br_
        , HH.label_ [ HH.text "Select genotype data file(s): " ]
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
      snpWeightData <- liftEffect $ readSnpWeights content
      H.modify_ _ { nrSnps = snpWeightData.numSnps, statusWeightFileLoading = false }
    Nothing -> pure unit

handleAction (GotGenoDataFileEvent _) = pure unit
