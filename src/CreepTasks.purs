module CreepTasks 
  ( upgradeNearestController
  , harvestEnergy
  , buildNextConstructionSite
  , deliverToClosestStructure) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_my_structures, find_sources_active, resource_energy)
import Screeps.Constants (find_ruins)
import Screeps.Creep (build, harvestSource, moveTo, say, transferToStructure, upgradeController, withdraw)
import Screeps.Extension (toExtension)
import Screeps.Extension as Extension
import Screeps.Room (controller, find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath, findClosestByPath')
import Screeps.Ruin as Ruin
import Screeps.Spawn (toSpawn)
import Screeps.Spawn as Spawn
import Screeps.Tower (toTower)
import Screeps.Tower as Tower
import Screeps.Types (Creep, FindContext(..), Ruin, Structure, TargetPosition(..))
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
  closestRuin <- findClosestByPath' (pos creep) (OfType find_ruins) (closestPathOpts { filter = Just hasEnergy })
  
  case closestRuin of
    Just ruin -> do
      harvestResult <- withdraw creep ruin resource_energy
      if harvestResult == err_not_in_range
      then creep `moveTo` (TargetObj ruin) # ignoreM
      else pure unit
    Nothing -> do
      closestSource <- findClosestByPath (pos creep) (OfType find_sources_active) 
      case closestSource of
        Nothing -> creep `say` "I'm stuck" # ignoreM
        Just targetSource -> do
          harvestResult <- creep `harvestSource` targetSource
          if harvestResult == err_not_in_range
          then creep `moveTo` (TargetObj targetSource) # ignoreM
          else pure unit
  
  where
    hasEnergy :: Ruin -> Boolean
    hasEnergy ruin = Ruin.energy ruin > 0


deliverToClosestStructure :: Creep -> Effect Unit
deliverToClosestStructure creep = do
  closestStructure <- findClosestByPath' (pos creep) (OfType find_my_structures) (closestPathOpts { filter = Just structureFilter })
  case closestStructure of
    Just (energyStructure :: forall a. Structure a) -> do
      transferResult <- transferToStructure creep energyStructure resource_energy
      if transferResult == err_not_in_range
      then creep `moveTo` (TargetObj energyStructure) # ignoreM
      else pure unit
    Nothing -> buildNextConstructionSite creep

  where
    structureFilter :: (forall a. Structure a) -> Boolean
    structureFilter x 
      | Just spawn <- toSpawn x = Spawn.energy spawn < Spawn.energyCapacity spawn
      | Just extension <- toExtension x = Extension.energy extension < Extension.energyCapacity extension
      | Just tower <- toTower x = Tower.energy tower < Tower.energyCapacity tower
      | otherwise = false 
      



buildNextConstructionSite :: Creep -> Effect Unit
buildNextConstructionSite creep = 
  case head (find (room creep) find_construction_sites) of
    Nothing -> upgradeNearestController creep
    Just targetSite -> do
      buildResult <- creep `build` targetSite
      if buildResult == err_not_in_range 
      then creep `moveTo` (TargetObj targetSite) # ignoreM
      else pure unit
