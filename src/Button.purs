module Button where

import Prelude

import Data.Maybe (Maybe(..))
-- import Data.String (split, Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.File.File (File, name, size)
import Web.File.FileList (item)
import Web.HTML.HTMLInputElement (fromEventTarget, files)

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
      -- H.liftAff $ do
      --   (contents, size) <- readFileAsText file
      --   let lineCount = length $ split (Pattern "\n") contents
      --   H.modify_ _ { fileNrLines = Just { size: size, lines: lineCount } }
    Nothing -> pure unit

