module PCproject.PlinkData where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer, Uint32Array, Uint8Array)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)

type PlinkBimData =
  { snpIDs :: Array String
  , chromosomes :: Uint8Array
  , positions :: Uint32Array
  , alleles1 :: Uint8Array
  , alleles2 :: Uint8Array
  }

type PlinkFamData =
  { indNames :: Array String
  , popNames :: Array String
  }

type PlinkBedData = Uint8Array

type PlinkData =
  { bimData :: PlinkBimData
  , famData :: PlinkFamData
  , bedData :: PlinkBedData
  , numIndividuals :: Int
  , numSNPs :: Int
  }

foreign import readBimDataImpl :: EffectFn1 String PlinkBimData
foreign import readFamDataImpl :: EffectFn1 String PlinkFamData
foreign import readBedDataImpl :: EffectFn3 ArrayBuffer Int Int PlinkBedData

readBimData :: forall m. MonadEffect m => String -> m PlinkBimData
readBimData bimContent = liftEffect $runEffectFn1 readBimDataImpl bimContent
readFamData :: forall m. MonadEffect m => String -> m PlinkFamData
readFamData famContent = liftEffect $ runEffectFn1 readFamDataImpl famContent
readBedData :: forall m. MonadEffect m => ArrayBuffer -> Int -> Int -> m PlinkBedData
readBedData bedArrayBuffer numSnps numInds = liftEffect $ runEffectFn3 readBedDataImpl bedArrayBuffer numSnps numInds

-- checkBedFileMagicBytes :: forall m. MonadEffect m => ArrayBuffer -> m Boolean
-- checkBedFileMagicBytes arr = do
--   bytes <- liftEffect $ whole arr :: m Uint8Array
--   maybeFirstThree <- sequence [bytes ! 0, bytes ! 1, bytes ! 2]
--   case map (map toInt) $ sequence maybeFirstThree of
--     Just [108, 27, 1] -> pure true -- magic bytes are [0b01101100, 0b00011011, 0b00000001]
--     _ -> pure false