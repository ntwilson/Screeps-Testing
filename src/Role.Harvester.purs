module Role.Harvester (run) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object (lookup)
import Screeps (err_not_in_range, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, transferToStructure)
import Screeps.Game (getGameGlobal, spawns)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Types (Creep, ResourceType(..), TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

run :: Creep -> Effect Unit
run creep =

  if amtCarrying creep (ResourceType "energy") < carryCapacity creep
  then
    case head (find (room creep) find_sources) of
      Nothing -> pure unit
      Just targetSource -> do
        harvestResult <- harvestSource creep targetSource
        if harvestResult == err_not_in_range
        then moveTo creep (TargetObj targetSource) # ignoreM
        else pure unit
        
  else do
    game <- getGameGlobal
    case (spawns game # lookup "Spawn1") of
      Nothing -> pure unit
      Just spawn1 -> do
        transferResult <- transferToStructure creep spawn1 resource_energy
        if transferResult == err_not_in_range
        then moveTo creep (TargetObj spawn1) # ignoreM
        else pure unit
