module CreepTasks 
  ( upgradeNearestController
  , harvestEnergy
  , buildNextConstructionSite
  , deliverToClosestStructure
  , repairNearestStructure) where

import Prelude

import Data.Array (head)
import Data.Foldable (minimumBy)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_my_structures, find_sources_active, find_structures, resource_energy)
import Screeps.Constants (find_ruins, find_tombstones)
import Screeps.Creep (build, harvestSource, moveTo, repair, transferToStructure, upgradeController, withdraw, withdrawFromTombstone)
import Screeps.Extension (toExtension)
import Screeps.Extension as Extension
import Screeps.Room (controller, find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath, findClosestByPath')
import Screeps.Ruin as Ruin
import Screeps.Spawn (toSpawn)
import Screeps.Spawn as Spawn
import Screeps.Structure (hits, hitsMax)
import Screeps.Tower (toTower)
import Screeps.Tower as Tower
import Screeps.Types (Creep, FindContext(..), Ruin, Structure, TargetPosition(..), Tombstone)
import Store (getUsedCapacity)
import Tombstone (store)
import Util (ignore)


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
  closestRuin <- findClosestByPath' (pos creep) (OfType find_ruins) (closestPathOpts { filter = Just ruinHasEnergy })
  
  case closestRuin of
    Just ruin -> do
      harvestResult <- withdraw creep ruin resource_energy
      if harvestResult == err_not_in_range
      then creep `moveTo` (TargetObj ruin) <#> const true
      else pure true
    Nothing -> do
      closestTombstoneWithEnergy <- findClosestByPath' (pos creep) (OfType find_tombstones) (closestPathOpts { filter = Just tombstoneHasEnergy })
      case closestTombstoneWithEnergy of
        Just tombstone -> do
          harvestResult <- withdrawFromTombstone creep tombstone resource_energy
          if harvestResult == err_not_in_range
          then creep `moveTo` (TargetObj tombstone) <#> const true
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
    ruinHasEnergy :: Ruin -> Boolean
    ruinHasEnergy ruin = Ruin.energy ruin > 0

    tombstoneHasEnergy :: Tombstone -> Boolean
    tombstoneHasEnergy tombstone  
      | Just energy <- (store tombstone) `getUsedCapacity` resource_energy 
      , energy > 0 = true
      | otherwise = false
        


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
  nearestDamagedTower <- findClosestByPath' (pos creep) (OfType find_my_structures) (closestPathOpts { filter = Just isDamagedTower })
  case nearestDamagedTower of 
    Just (structure :: forall a. Structure a) -> repairIt structure <#> const true
    Nothing -> repairMostDamagedStructure
  
  where 
    isDamagedTower (structure :: forall a. Structure a)
      | Just tower <- toTower structure = hits tower < hitsMax tower
      | otherwise = false  

    repairIt :: (forall a. Structure a) -> Effect Unit
    repairIt structure = do 
      repairResult <- creep `repair` structure
      if repairResult == err_not_in_range
      then creep `moveTo` (TargetObj structure) <#> ignore
      else pure unit

    repairMostDamagedStructure :: Effect Boolean
    repairMostDamagedStructure =
      case mostDamagedStructure of
        Just (struct :: forall a. Structure a) -> repairIt struct <#> const true
        Nothing -> pure false

    mostDamagedStructure :: Maybe (forall a. Structure a)
    mostDamagedStructure = 
      let 
        allStructures = find (room creep) find_structures 
        structureHealths = 
          allStructures 
          <#> \a -> { structure: (a :: forall b. Structure b), health: toNumber (hits a) / toNumber (hitsMax a) }

      in
        structureHealths 
        # minimumBy (\{health: healthA} {health: healthB} -> compare healthA healthB)
        # map _.structure
