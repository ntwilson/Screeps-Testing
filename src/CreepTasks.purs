module CreepTasks where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_my_structures, find_sources_active, resource_energy, structure_extension, structure_spawn)
import Screeps.Creep (build, harvestSource, moveTo, say, transferToStructure, upgradeController)
import Screeps.Room (controller, find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath, findClosestByPath')
import Screeps.Spawn as Spawn
import Screeps.Structure (structureType)
import Screeps.Types (Creep, FindContext(..), RawRoomObject, RawStructure, Spawn, TargetPosition(..))
import Util (ignoreM)


upgradeNearestController :: Creep -> Effect Unit
upgradeNearestController creep =
  case (controller (room creep)) of
    Nothing -> creep `say` "I'm stuck" # ignoreM
    Just controller -> do
      upgradeResult <- creep `upgradeController` controller
      if upgradeResult == err_not_in_range
      then creep `moveTo` (TargetObj controller) # ignoreM
      else pure unit

harvestEnergy :: Creep -> Effect Unit
harvestEnergy creep = do
  closestSource <- findClosestByPath (pos creep) (OfType find_sources_active) 
  case closestSource of
    Nothing -> creep `say` "I'm stuck" # ignoreM
    Just targetSource -> do
      harvestResult <- creep `harvestSource` targetSource
      if harvestResult == err_not_in_range
      then creep `moveTo` (TargetObj targetSource) # ignoreM
      else pure unit


deliverToClosestStructure :: Creep -> Effect Unit
deliverToClosestStructure creep = do
  closestStructure <- findClosestByPath' (pos creep) (OfType find_my_structures) (closestPathOpts { filter = structureFilter })
  case closestStructure of
    Just spawn -> do
      transferResult <- transferToStructure creep spawn resource_energy
      if transferResult == err_not_in_range
      then creep `moveTo` (TargetObj spawn) # ignoreM
      else pure unit
    Nothing -> creep `say` "I'm stuck." # ignoreM

  where
    structureFilter :: Maybe (Spawn -> Boolean)
    structureFilter = Just (\x -> desiredTarget x && Spawn.energy x < Spawn.energyCapacity x)

    desiredTarget :: forall a. RawRoomObject (RawStructure a) -> Boolean
    desiredTarget struct = 
      (structureType struct) == structure_spawn || (structureType struct) == structure_extension

buildNextConstructionSite :: Creep -> Effect Unit
buildNextConstructionSite creep = 
  case head (find (room creep) find_construction_sites) of
    Nothing -> do
      creep `say` "switching to upgrade" # ignoreM
      upgradeNearestController creep
    Just targetSite -> do
      buildResult <- creep `build` targetSite
      if buildResult == err_not_in_range 
      then creep `moveTo` (TargetObj targetSite) # ignoreM
      else pure unit
