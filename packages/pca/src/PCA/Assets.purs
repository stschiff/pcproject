module PCA.Assets
  ( PanelRef
  , AssetFetcher
  , defaultFetcher
  , loadReferenceBundle
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Either (Either(..), either)
import Effect.Aff (Aff, try)
import Effect.Exception (error, message)
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

loadReferenceBundle :: AssetFetcher -> PanelRef -> Aff (Either String ReferenceBundle)
loadReferenceBundle fetcher panel = do
  result <- try do
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
  pure case result of
    Left err -> Left (message err)
    Right rb -> Right rb
