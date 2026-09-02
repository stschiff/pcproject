-- | The npm/JS bundle entry point for this package (see pca/package.json's
-- | build script). PCA and PCA.Assets stay idiomatic PureScript (Aff,
-- | Data.Map, curried Effect functions) for PureScript-native consumers
-- | like the webapp - this module is a thin conversion layer on top, doing
-- | the one-time Promise/Object/EffectFn conversion that plain JS/npm/
-- | Observable consumers actually need, without pushing those compromises
-- | into the core API. Every function exported from here is a plain,
-- | directly-callable JS function (no `fn(args)()`, no currying) via
-- | Effect.Uncurried's EffectFnN/mkEffectFnN.
-- |
-- | module PCA / module PCA.Assets (rather than naming e.g. PanelRef
-- | individually) is required here, not just a style choice: PureScript
-- | can't re-export an imported `type` synonym by bare name, only via a
-- | full module wildcard.
module PCA.Interop
  ( module PCA
  , module PCA.Assets
  , panels
  , loadReferenceBundle
  , getOverlapMasks
  , reducePcWeights
  , extractAndTransposeGenotypes
  , projectSamples
  ) where

import Data.ArrayBuffer.Types (Float32Array, Uint8Array)
import Data.Either (Either)
import Data.Map as Map
import Data.Tuple (Tuple)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, EffectFn6,
        mkEffectFn1, mkEffectFn2, mkEffectFn4, mkEffectFn6)
import Foreign.Object (Object)
import Foreign.Object as Object
import Promise (Promise)
import Promise.Aff (fromAff)

import GenotypeIO (PlinkBimData)
import PCA (OverlapMasks, PCAparams, ProjectionResult, RefPosData, ReferenceBundle,
        SampleData, SnpWeights, readRefPosData, readSnpWeights)
import PCA as Core
import PCA.Assets (AssetFetcher, PanelRef)
import PCA.Assets as Assets

-- | Same panels as PCA.Assets.panels, as a plain JS object keyed by name
-- | (e.g. `panels["WestEurasia_HiRes"]`) instead of an opaque Data.Map.
panels :: Object PanelRef
panels = Object.fromFoldable (Map.toUnfoldable Assets.panels :: Array (Tuple String PanelRef))

-- | Loads a panel with the default fetcher. `await loadReferenceBundle(panel)`
-- | from JS - no trailing `()`, it hands back the Promise directly. Custom
-- | AssetFetchers (e.g. a caching layer) aren't exposed here - use
-- | PCA.Assets directly from PureScript for that.
loadReferenceBundle :: EffectFn1 PanelRef (Promise (Either String ReferenceBundle))
loadReferenceBundle = mkEffectFn1 \panel -> fromAff (Assets.loadReferenceBundle Assets.defaultFetcher panel)

getOverlapMasks :: EffectFn2 PlinkBimData SnpWeights OverlapMasks
getOverlapMasks = mkEffectFn2 Core.getOverlapMasks

reducePcWeights :: EffectFn2 SnpWeights OverlapMasks SnpWeights
reducePcWeights = mkEffectFn2 Core.reducePcWeights

extractAndTransposeGenotypes :: EffectFn4 Uint8Array Int Int OverlapMasks Uint8Array
extractAndTransposeGenotypes = mkEffectFn4 Core.extractAndTransposeGenotypes

projectSamples :: EffectFn6 Uint8Array Float32Array Float32Array Int Int PCAparams (Array ProjectionResult)
projectSamples = mkEffectFn6 Core.projectSamples
