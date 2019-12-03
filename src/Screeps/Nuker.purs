-- | Corresponds to the Screeps API [StructureNuker](http://support.screeps.com/hc/en-us/articles/208488255-StructureNuker)
module Screeps.Nuker where

import Effect (Effect)
import Data.Maybe (Maybe)

import Screeps.Constants (structure_nuker)
import Screeps.FFI (runThisEffFn1, unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (Nuker, ReturnCode, RoomPosition, class Structure)

cooldown :: Nuker -> Int
cooldown = unsafeField "cooldown"

launchNuke :: Nuker -> RoomPosition -> Effect ReturnCode
launchNuke = runThisEffFn1 "launchNuke"

toNuker :: forall a. Structure a => a -> Maybe Nuker
toNuker = unsafeCast structure_nuker
