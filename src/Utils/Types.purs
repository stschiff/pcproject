module Utils.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Web.File.File (File)

data GenoData = GenoDataPlink File File File
              | GenoDataEigenstrat File File File
              | GenoDataVcf File

data SnpWeight = SnpWeight
  { snpId :: String
  , chromosome :: String
  , position :: Int
  , pcWeights :: Array Number
  }

derive instance genericSnpWeight :: Generic SnpWeight _

instance showSnpWeight :: Show SnpWeight where
  show = genericShow


