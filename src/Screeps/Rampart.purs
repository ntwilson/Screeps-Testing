-- | Corresponds to the Screeps API [StructureRampart](http://support.screeps.com/hc/en-us/articles/207712959-StructureRampart)
module Screeps.Rampart where

import Effect (Effect)
import Data.Maybe (Maybe)

import Screeps.Constants (structure_rampart)
import Screeps.FFI (unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (Rampart, ReturnCode, class Structure)

isPublic :: Rampart -> Boolean
isPublic = unsafeField "isPublic"

ticksToDecay :: Rampart -> Int
ticksToDecay = unsafeField "ticksToDecay"

setPublic :: Rampart -> Boolean -> Effect ReturnCode
setPublic = unsafeField "setPublic"

toRampart :: forall a. Structure a => a -> Maybe Rampart
toRampart = unsafeCast structure_rampart
