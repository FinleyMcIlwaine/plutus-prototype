module Monaco where

import Prelude

import Effect (Effect)

foreign import data Monaco :: Type

foreign import getMonaco :: Effect Monaco

foreign import create_ :: Monaco -> String -> Effect Unit