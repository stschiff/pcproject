module Button where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Data.Either (Either(..))
import Data.Array (length)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (readString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.File.File (File, name, size, toBlob)
import Web.File.FileList (item)
import Web.File.FileReader (fileReader, readAsText, result, toEventTarget)
import Web.HTML.HTMLInputElement (fromEventTarget, files)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.Event.EventTypes (load, error) as ET

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
  { selectedFile :: Maybe File
  , fileNrLines :: Maybe Int
  }

data Action
  = GotFileEvent WE.Event

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState = const { selectedFile: Nothing, fileNrLines : Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form_
    [ HH.input 
      [ HP.type_ HP.InputFile, HE.onChange GotFileEvent ]
    , case st.selectedFile of
        Nothing -> HH.text "No file selected"
        Just file -> HH.div_
          [ HH.text $ "Selected file: " <> name file
          , HH.br_
          , HH.text $ "File size: " <> show (size file) <> " bytes"
          ]
    , case st.fileNrLines of
        Nothing -> HH.text ""
        Just nrLines -> HH.text $ "Lines: " <> show nrLines <> " lines"
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction (GotFileEvent ev) = do
  let mInputElem = WE.target ev >>= fromEventTarget
  mFile <- case mInputElem of
    Nothing -> pure Nothing
    Just inputElem -> do
      mFileList <- liftEffect $ files inputElem
      pure $ mFileList >>= item 0
  case mFile of
    Just file -> do
      H.modify_ _ { selectedFile = Just file }
      -- Read file contents asynchronously using makeAff
      content <- readFileAsTextAff file
      let lineCount = length $ split (Pattern "\n") content
      H.modify_ _ { fileNrLines = Just lineCount }
    Nothing -> pure unit

