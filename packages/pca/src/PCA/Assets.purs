module PCA.Assets
  ( PanelRef
  , AssetFetcher
  , defaultFetcher
  , loadReferenceBundle
  , panels
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Either (either)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Fetch (fetch)

import PCA (ReferenceBundle, readRefPosData, readSnpWeights)

-- | Everything needed to locate one hosted PCA reference panel: three plain
-- | URLs. Deliberately host-agnostic (no assumption of GitHub Releases, a
-- | CDN, or same-origin hosting) so each consumer (webapp, CLI, ...) can
-- | point it at whatever's appropriate for its own environment.
type PanelRef =
  { name :: String
  , weightsUrl :: String
  , evecUrl :: String
  , paramsUrl :: String
  }

-- | Fetches the body of a URL as text, or fails the Aff. Injectable so
-- | consumers can wrap it with e.g. a caching layer without this package
-- | needing to know about IndexedDB, disk caches, etc.
type AssetFetcher = String -> Aff String

-- | Fetches via the global `fetch`, available unmodified in both browsers
-- | and Node >=18 - CORS (a browser-only restriction) is the caller's
-- | concern via which URLs a PanelRef points at, not this function's.
defaultFetcher :: AssetFetcher
defaultFetcher url = do
  response <- fetch url {}
  if response.ok
    then response.text
    else throwError $ error $
      "Failed to fetch " <> url <> " (HTTP " <> show response.status <> ")"

-- | Fails the Aff (via throwError) on any fetch or parse error, rather than
-- | returning an Either - use `try`/`attempt` at the call site if you need
-- | to branch on success/failure, matching the rest of this codebase (see
-- | App.UserInputComponent's use of `attempt`). This also means PCA.Interop
-- | gets a rejected Promise for free via Promise.Aff.fromAff, with no
-- | Either to unwrap on the JS side.
loadReferenceBundle :: AssetFetcher -> PanelRef -> Aff ReferenceBundle
loadReferenceBundle fetcher panel = do
  weightsText <- fetcher panel.weightsUrl
  evecText <- fetcher panel.evecUrl
  paramsText <- fetcher panel.paramsUrl
  pcaParams <- either (throwError <<< error <<< printJsonDecodeError) pure
    (parseJson paramsText >>= decodeJson)
  pure
    { snpWeights: readSnpWeights weightsText
    , refPosData: readRefPosData evecText
    , pcaParams
    }

-- | Known hosted PCA reference panels, keyed by name. Add an entry here
-- | whenever a new panel is published to the assets host - this is the one
-- | place consumers (webapp, future CLI) look panels up by name from.
panels :: Map String PanelRef
panels = Map.fromFoldable
  [ Tuple "WestEurasia_HiRes"
      { name: "WestEurasia_HiRes"
      , weightsUrl: assetsBaseUrl <> "pcproject/WestEurasia_HiRes/WestEurasia_HiRes_weights_with_freqs.txt"
      , evecUrl: assetsBaseUrl <> "pcproject/WestEurasia_HiRes/WestEurasia_HiRes_evec_with_groups.tsv"
      , paramsUrl: assetsBaseUrl <> "pcproject/WestEurasia_HiRes/WestEurasia_HiRes_parameters.json"
      }
  ]
  where
  assetsBaseUrl = "https://assets.stephanschiffels.de/"
