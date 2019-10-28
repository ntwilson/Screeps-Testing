module CreepTasks 
  ( upgradeNearestController
  , harvestEnergy
  , buildNextConstructionSite
  , deliverToClosestStructure
  , repairNearestStructure) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_my_structures, find_sources_active, find_structures, ok, resource_energy)
import Screeps.Constants (find_ruins)
import Screeps.Creep (build, harvestSource, moveTo, repair, transferToStructure, upgradeController, withdraw)
import Screeps.Extension (toExtension)
import Screeps.Extension as Extension
import Screeps.Rampart (toRampart)
import Screeps.Road (toRoad)
import Screeps.Room (controller, find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath, findClosestByPath')
import Screeps.Ruin as Ruin
import Screeps.Spawn (toSpawn)
import Screeps.Spawn as Spawn
import Screeps.Structure (hits, hitsMax)
import Screeps.Tower (toTower)
import Screeps.Tower as Tower
import Screeps.Types (Creep, FindContext(..), Ruin, Structure, TargetPosition(..))
import Screeps.Wall (toWall)


upgradeNearestController :: Creep -> Effect Boolean
upgradeNearestController creep =
  case (controller (room creep)) of
    Nothing -> pure false
    Just controller -> do
      upgradeResult <- creep `upgradeController` controller
      if upgradeResult == err_not_in_range
      then creep `moveTo` (TargetObj controller) <#> const true
      else pure true
  
harvestEnergy :: Creep -> Effect Boolean
harvestEnergy creep = do
  closestRuin <- findClosestByPath' (pos creep) (OfType find_ruins) (closestPathOpts { filter = Just hasEnergy })
  
  case closestRuin of
    Just ruin -> do
      harvestResult <- withdraw creep ruin resource_energy
      if harvestResult == err_not_in_range
      then creep `moveTo` (TargetObj ruin) <#> const true
      else pure true
    Nothing -> do
      closestSource <- findClosestByPath (pos creep) (OfType find_sources_active) 
      case closestSource of
        Nothing -> pure false
        Just targetSource -> do
          harvestResult <- creep `harvestSource` targetSource
          if harvestResult == err_not_in_range
          then creep `moveTo` (TargetObj targetSource) <#> const true
          else pure true
  
  where
    hasEnergy :: Ruin -> Boolean
    hasEnergy ruin = Ruin.energy ruin > 0


deliverToClosestStructure :: Creep -> Effect Boolean
deliverToClosestStructure creep = do
  closestStructure <- findClosestByPath' (pos creep) (OfType find_my_structures) (closestPathOpts { filter = Just structureFilter })
  case closestStructure of
    Just (energyStructure :: forall a. Structure a) -> do
      transferResult <- transferToStructure creep energyStructure resource_energy
      if transferResult == err_not_in_range
      then creep `moveTo` (TargetObj energyStructure) <#> const true
      else pure true
    Nothing -> pure false

  where
    structureFilter :: (forall a. Structure a) -> Boolean
    structureFilter x 
      | Just spawn <- toSpawn x = Spawn.energy spawn < Spawn.energyCapacity spawn
      | Just extension <- toExtension x = Extension.energy extension < Extension.energyCapacity extension
      | Just tower <- toTower x = Tower.energy tower < Tower.energyCapacity tower
      | otherwise = false 
      



buildNextConstructionSite :: Creep -> Effect Boolean
buildNextConstructionSite creep = 
  case head (find (room creep) find_construction_sites) of
    Nothing -> pure false
    Just targetSite -> do
      buildResult <- creep `build` targetSite
      if buildResult == err_not_in_range 
      then creep `moveTo` (TargetObj targetSite) <#> const true
      else pure true

repairNearestStructure :: Creep -> Effect Boolean
repairNearestStructure creep = do
  repairResult1 <- repairNearest toTower
  if repairResult1 then pure true
  else do
    repairResult2 <- repairNearest toRoad
    if repairResult2 then pure true
    else do
      repairResult3 <- repairNearest toRampart
      if repairResult3 then pure true
      else repairNearest toWall

  where 
    repairNearest :: forall genericStructure specificStructure. (Structure genericStructure -> Maybe specificStructure) -> Effect Boolean
    repairNearest tryClassifyStructure = do
      let filter (a :: forall x. Structure x) = isJust (tryClassifyStructure a) && (hits a < hitsMax a)
      nearestStructure <- findClosestByPath' (pos creep) (OfType find_structures) (closestPathOpts { filter = Just filter })
      case nearestStructure of 
        Just (structure :: forall a. Structure a) -> creep `repair` structure <#> (_ == ok)
        Nothing -> pure false

