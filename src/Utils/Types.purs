module Utils.Types where

import Prelude

import Web.File.File (File)

data GenoData = GenoDataPlink File File File
              | GenoDataEigenstrat File File File
              | GenoDataVcf File

data SnpWeights =
  { snpIds      :: Array String
  , chromosomes :: Array String
  , positions   :: UInt32Array
  , pcWeights   :: Float16Array
  , numSnps     :: Int
  , numPCs.     :: Int
  }


