module Types where

import Data.Newtype (class Newtype)
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Query a

data HAction
  = Init

newtype State
  = State
  { 
  }

derive instance newtypeState :: Newtype State _

derive instance genericState :: Generic State _

instance showState :: Show State where
  show = genericShow
