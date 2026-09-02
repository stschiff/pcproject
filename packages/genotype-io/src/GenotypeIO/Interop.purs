-- | The npm/JS bundle entry point for this package (see genotype-io/
-- | package.json's build script). GenotypeIO stays idiomatic PureScript
-- | (curried Effect functions) for PureScript-native consumers like the
-- | webapp - this module wraps each function as an EffectFn via
-- | Effect.Uncurried so JS/npm/Observable consumers get plain, directly
-- | callable functions instead (`readFamData(text)`, not
-- | `readFamData(text)()`). See PCA.Interop in the pca package for the
-- | same pattern.
module GenotypeIO.Interop
  ( module GenotypeIO
  , readBimData
  , readFamData
  , readBedData
  ) where

import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, mkEffectFn3)

import GenotypeIO hiding (readBimData, readFamData, readBedData)
import GenotypeIO as Core

readBimData :: EffectFn1 String PlinkBimData
readBimData = mkEffectFn1 Core.readBimData

readFamData :: EffectFn1 String PlinkFamData
readFamData = mkEffectFn1 Core.readFamData

readBedData :: EffectFn3 ArrayBuffer Int Int PlinkBedData
readBedData = mkEffectFn3 Core.readBedData
