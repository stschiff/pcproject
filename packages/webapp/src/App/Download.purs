module App.Download (downloadCSV) where

import Prelude (Unit)
import Effect (Effect)

-- Triggers a browser file-save of `content` as a CSV named `filename`, via a
-- throwaway Blob/ObjectURL/anchor-click -- there's no PureScript binding for
-- URL.createObjectURL in this workspace's dependencies, so this stays FFI.
foreign import downloadCSV :: String -> String -> Effect Unit
