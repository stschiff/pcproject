module PCproject.PlinkData where

import Web.File.File (File)

data GenoData = GenoDataPlink File File File
              | GenoDataEigenstrat File File File
              | GenoDataVcf File



