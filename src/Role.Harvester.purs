module Role.Harvester (run) where

import Prelude

import Data.Array.Partial (head)
import Data.Maybe (fromJust)
import Effect (Effect)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Screeps (err_not_in_range, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, transferToStructure)
import Screeps.Game (getGameGlobal, spawns)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Types (Creep, ResourceType(..), TargetPosition(..))

run :: Creep -> Effect Unit
run creep = unsafePartial do
  game <- getGameGlobal

  if amtCarrying creep (ResourceType "energy") < carryCapacity creep
  then do
    let sources = find (room creep) find_sources
    let targetSource = head sources
    harvestResult <- harvestSource creep targetSource
    if harvestResult == err_not_in_range
    then moveTo creep (TargetObj targetSource) <#> (\_ -> unit)
    else pure unit

  else do
    let spawn1 = (spawns game # lookup "Spawn1" # fromJust)
    transferResult <- transferToStructure creep spawn1 resource_energy
    if transferResult == err_not_in_range
    then moveTo creep (TargetObj spawn1) <#> (\_ -> unit)
    else pure unit
