module PCproject.Plot where

import Effect (Effect)
import Prelude
import PCproject.RefPosData (RefPosData)

foreign import drawChart :: RefPosData -> Int -> Int -> Effect Unit
