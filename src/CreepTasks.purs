module CreepTasks 
  ( upgradeNearestController
  , harvestEnergy
  , buildNextConstructionSite
  , deliverToClosestStructure
  , repairNearestStructure) where

import Prelude

import Classes (energy, energyCapacity, hits, hitsMax)
import Data.Array (head, index, sortBy)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_my_structures, find_sources_active, find_structures, resource_energy)
import Screeps.Constants (find_ruins, find_tombstones)
import Screeps.Creep (build, harvestSource, moveTo, repair, transferToStructure, upgradeController, withdraw, withdrawFromTombstone)
import Screeps.Extension (toExtension)
import Screeps.Room (controller, find, find')
import Screeps.RoomObject (pos, room, targetObj)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath, findClosestByPath', getRangeTo)
import Screeps.Spawn (toSpawn)
import Screeps.Tower (toTower)
import Screeps.Types (class Structure, Creep, Ruin, SomeStructure, Tombstone)
import Util (ignore)


upgradeNearestController :: Creep -> Effect Boolean
upgradeNearestController creep =
  case (controller (room creep)) of
    Nothing -> pure false
    Just controller -> do
      upgradeResult <- creep `upgradeController` controller
      if upgradeResult == err_not_in_range
      then creep `moveTo` (targetObj controller) <#> const true
      else pure true
  
harvestEnergy :: Creep -> Effect Boolean
harvestEnergy creep = do
  closestRuin <- findClosestByPath' (pos creep) find_ruins (closestPathOpts { filter = Just ruinHasEnergy })
  
  case closestRuin of
    Just ruin -> do
      harvestResult <- withdraw creep ruin resource_energy
      if harvestResult == err_not_in_range
      then creep `moveTo` (targetObj ruin) <#> const true
      else pure true
    Nothing -> do
      closestTombstoneWithEnergy <- findClosestByPath' (pos creep) find_tombstones (closestPathOpts { filter = Just tombstoneHasEnergy })
      case closestTombstoneWithEnergy of
        Just tombstone -> do
          harvestResult <- withdrawFromTombstone creep tombstone resource_energy
          if harvestResult == err_not_in_range
          then creep `moveTo` (targetObj tombstone) <#> const true
          else pure true
        Nothing -> do 
          closestSource <- findClosestByPath (pos creep) find_sources_active 
          case closestSource of
            Nothing -> pure false
            Just targetSource -> do
              harvestResult <- creep `harvestSource` targetSource
              if harvestResult == err_not_in_range
              then creep `moveTo` (targetObj targetSource) <#> const true
              else pure true
  
  where
    ruinHasEnergy :: Ruin -> Boolean
    ruinHasEnergy ruin = energy ruin > 0

    tombstoneHasEnergy :: Tombstone -> Boolean
    tombstoneHasEnergy tombstone  
      | energyCapacity tombstone > 0 = true
      | otherwise = false
        


deliverToClosestStructure :: Creep -> Effect Boolean
deliverToClosestStructure creep = do
  closestStructure <- findClosestByPath' (pos creep) find_my_structures (closestPathOpts { filter = Just structureFilter })
  case closestStructure of
    Just energyStructure -> do
      transferResult <- transferToStructure creep energyStructure resource_energy
      if transferResult == err_not_in_range
      then creep `moveTo` (targetObj energyStructure) <#> const true
      else pure true
    Nothing -> pure false

  where
    structureFilter :: SomeStructure -> Boolean
    structureFilter x 
      | Just spawn <- toSpawn x = energy spawn < energyCapacity spawn
      | Just extension <- toExtension x = energy extension < energyCapacity extension
      | Just tower <- toTower x = energy tower < energyCapacity tower
      | otherwise = false 
      



buildNextConstructionSite :: Creep -> Effect Boolean
buildNextConstructionSite creep = 
  case head (find (room creep) find_construction_sites) of
    Nothing -> pure false
    Just targetSite -> do
      buildResult <- creep `build` targetSite
      if buildResult == err_not_in_range 
      then creep `moveTo` (targetObj targetSite) <#> const true
      else pure true

repairNearestStructure :: Creep -> Effect Boolean
repairNearestStructure creep = do
  nearestDamagedTower <- findClosestByPath' (pos creep) find_my_structures (closestPathOpts { filter = Just isDamagedTower })
  case nearestDamagedTower of 
    Just structure -> repairIt structure <#> const true
    Nothing -> repairMostDamagedStructure
  
  where 
    isDamagedTower structure
      | Just tower <- toTower structure = hits tower < hitsMax tower
      | otherwise = false  

    repairIt :: forall a. Structure a => a -> Effect Unit
    repairIt structure = do 
      repairResult <- repair creep structure
      if repairResult == err_not_in_range
      then creep `moveTo` (targetObj structure) <#> ignore
      else pure unit

    repairMostDamagedStructure :: Effect Boolean
    repairMostDamagedStructure =
      -- don't bounce back and forth between two structures.  If you're already repairing one structure
      -- stick with it even if it becomes no longer the most damaged structure.
      case secondMostDamaged of
        Just struct 
          | (pos creep) `getRangeTo` (targetObj struct) < 5 -> 
            repairIt struct <#> const true

        _ -> 
          case mostDamagedStructure of
            Just struct -> repairIt struct <#> const true
            Nothing -> pure false

    structuresInOrderOfHealth :: Array SomeStructure
    structuresInOrderOfHealth =
      let 
        allStructures = find' (room creep) find_structures (\struct -> hits struct < min (1_000_000) (hitsMax struct))
        structureHealths = 
          allStructures 
          <#> \a -> { structure: a, health: toNumber (hits a) / toNumber (min 1_000_000 (hitsMax a)) }
      in
        structureHealths 
          # sortBy (\{health: healthA} {health: healthB} -> compare healthA healthB)
          # map _.structure

    mostDamagedStructure :: Maybe SomeStructure
    mostDamagedStructure = head structuresInOrderOfHealth 

    secondMostDamaged :: Maybe SomeStructure
    secondMostDamaged = structuresInOrderOfHealth `index` 1
