module Button where

import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.File.File (File, name)
import Web.File.FileList (item)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLInputElement (fromEventTarget, files)

type State =
  { selectedFile :: Maybe File
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
initialState = const { selectedFile: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form_
    [ HH.input 
      [ HP.type_ HP.InputFile, HE.onChange GotFileEvent ]
    , case st.selectedFile of
        Nothing -> HH.text "No file selected"
        Just file -> HH.text $ "Selected file: " <> name file
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  GotFileEvent ev -> do
    case WE.target ev of
      Nothing -> pure unit
      Just t -> 
        case fromEventTarget t of
          Nothing -> pure unit
          Just inputElem -> do
            mFileList <- liftEffect $ files inputElem
            case mFileList of
              Nothing -> pure unit
              Just fileList -> do
                case item 0 fileList of
                  Just file -> H.modify_ _ { selectedFile = Just file }
                  Nothing -> pure unit

