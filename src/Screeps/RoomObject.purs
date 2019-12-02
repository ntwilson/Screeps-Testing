-- | Corresponds to the Screeps API [RoomObject](http://support.screeps.com/hc/en-us/articles/208435305-RoomObject)
module Screeps.RoomObject where

import Prelude

import Screeps.FFI (unsafeField)
import Screeps.Types (Room, class RoomObject, RoomPosition, TargetPosition(..))

room :: forall a. RoomObject a => a -> Room
room = unsafeField "room"

pos :: forall a. RoomObject a => a -> RoomPosition
pos = unsafeField "pos"

targetObj :: forall a. RoomObject a => a -> TargetPosition
targetObj x = TargetPos $ pos x
