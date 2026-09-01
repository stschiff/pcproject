-- | The npm/JS bundle entry point for this package (see pca/package.json's
-- | build script). PCA and PCA.Assets stay idiomatic PureScript (Aff,
-- | Data.Map) for PureScript-native consumers like the webapp - this module
-- | is a thin conversion layer on top, doing the one-time Promise/Object
-- | conversion that plain JS/npm/Observable consumers actually need,
-- | without pushing those compromises into the core API.
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
  ) where

import Data.Either (Either)
import Data.Map as Map
import Data.Tuple (Tuple)
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Promise (Promise)
import Promise.Aff (fromAff)

import PCA (OverlapMasks, PCAparams, ProjectionResult, RefPosData, ReferenceBundle,
        SampleData, SnpWeights, extractAndTransposeGenotypes, getOverlapMasks,
        projectSamples, readRefPosData, readSnpWeights, reducePcWeights)
import PCA.Assets hiding (panels, loadReferenceBundle, defaultFetcher)
import PCA.Assets as Assets

-- | Same panels as PCA.Assets.panels, as a plain JS object keyed by name
-- | (e.g. `panels["WestEurasia_HiRes"]`) instead of an opaque Data.Map.
panels :: Object PanelRef
panels = Object.fromFoldable (Map.toUnfoldable Assets.panels :: Array (Tuple String PanelRef))

-- | Loads a panel with the default fetcher and returns a real Promise, so
-- | JS callers can `await loadReferenceBundle(panel)()`. Custom AssetFetchers
-- | (e.g. a caching layer) aren't exposed here - use PCA.Assets directly
-- | from PureScript for that.
loadReferenceBundle :: PanelRef -> Effect (Promise (Either String ReferenceBundle))
loadReferenceBundle panel = fromAff (Assets.loadReferenceBundle Assets.defaultFetcher panel)
