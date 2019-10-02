module Role.Harvester (run) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Screeps (err_not_in_range, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, transferToStructure)
import Screeps.Game (getGameGlobal, spawns)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Types (Creep, ResourceType(..), TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreF :: forall f a. Functor f => f a -> f Unit
ignoreF f = f <#> ignore 

run :: Creep -> Effect Unit
run creep = do
  game <- getGameGlobal

  if amtCarrying creep (ResourceType "energy") < carryCapacity creep
  then
    case head (find (room creep) find_sources) of
      Nothing -> pure unit
      Just targetSource -> do
        harvestResult <- harvestSource creep targetSource
        if harvestResult == err_not_in_range
        then moveTo creep (TargetObj targetSource) # ignoreF
        else pure unit

  else
    case (spawns game # lookup "Spawn1") of
      Nothing -> pure unit
      Just spawn1 -> do
        transferResult <- transferToStructure creep spawn1 resource_energy
        if transferResult == err_not_in_range
        then moveTo creep (TargetObj spawn1) # ignoreF
        else pure unit
