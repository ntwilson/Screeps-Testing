-- | Corresponds to the Screeps API [StructureController](http://support.screeps.com/hc/en-us/articles/207711889-StructureController)
module Screeps.Controller where

import Data.Maybe (Maybe)
import Effect (Effect)

import Screeps.Constants (structure_controller)
import Screeps.FFI (runThisEffFn0, unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (Controller, ReturnCode, class Structure)

level :: Controller -> Int
level = unsafeField "level"

progress :: Controller -> Int
progress = unsafeField "progress"

progressTotal :: Controller -> Int
progressTotal = unsafeField "progressTotal"

reservation :: Controller -> Int
reservation = unsafeField "reservation"

ticksToDowngrade :: Controller -> Int
ticksToDowngrade = unsafeField "ticksToDowngrade"

upgradeBlocked :: Controller -> Int
upgradeBlocked = unsafeField "upgradeBlocked"

unclaim :: Controller -> Effect ReturnCode
unclaim = runThisEffFn0 "unclaim"

toController :: forall a. Structure a => a -> Maybe Controller
toController = unsafeCast structure_controller
