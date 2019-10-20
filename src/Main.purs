module Main (loop) where

import Prelude

import CreepClassification (CreepMemory(..), UnknownCreepType(..), VocationalCreep(..), classifyCreep, spawnCreep)
import CreepRoles (Role(..))
import Data.Array (fromFoldable, length, mapMaybe, zip)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (size)
import Role.Builder (runBuilder)
import Role.Builder as Builder
import Role.Guard (runGuard)
import Role.Guard as Guard
import Role.Harvester (runHarvester)
import Role.Harvester as Harvester
import Role.Upgrader (runUpgrader)
import Role.Upgrader as Upgrader
import Screeps (find_hostile_creeps, ok, part_move, structure_extension, structure_road)
import Screeps.Controller (level)
import Screeps.Game (creeps, getGameGlobal, spawns)
import Screeps.Room (controller, createConstructionSite, energyAvailable, energyCapacityAvailable, find)
import Screeps.RoomObject (room)
import Screeps.Types (BodyPartType, Creep, Room, Spawn, TargetPosition(..))
import Util (bodyPartCost, (<<#>>))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

noName :: Maybe String 
noName = Nothing

matchUnit :: Either UnknownCreepType VocationalCreep -> Effect Unit
matchUnit (Right (Harvester creep)) = runHarvester creep
matchUnit (Right (Upgrader creep)) = runUpgrader creep
matchUnit (Right (Builder creep)) = runBuilder creep
matchUnit (Right (Guard creep)) = runGuard creep
matchUnit (Left (UnknownCreepType err)) = log $ "One of the creeps has a memory I can't parse.\n" <> err

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = classifyCreep creep >>= matchUnit

shouldSpawnCreep :: { nCreeps :: Int, totalCapacity :: Int } -> Boolean
shouldSpawnCreep { nCreeps, totalCapacity } = 
  minBudget <= totalCapacity

  where
    minBudget 
      | nCreeps <= 5 = 300 -- max for room level 1
      | nCreeps <= 7 = 350
      | nCreeps <= 9 = 400
      | nCreeps <= 12 = 550 -- max for room level 2 
      | nCreeps <= 15 = 600 
      | nCreeps <= 18 = 800 -- max for room level 3
      | otherwise = 10000
    

constructionPlan :: Array (Array BodyPartType) -> Int -> Array BodyPartType
constructionPlan plans budget =
  let 
    costs = plans <<#>> bodyPartCost <#> sum
    maybePlan =
      zip plans costs 
      # Array.find (\(Tuple plan cost) -> cost <= budget)
      <#> (\(Tuple plan _cost) -> plan) 
  in fromMaybe [] maybePlan


-- NOTE: this is an ordered set.  Builders will build each structure
-- in the order listed here.
-- createConstructionSitesL1 :: Room -> Effect Unit
-- createConstructionSitesL1 room = 
--   let 
--     constructionSites = 
--       [ { loc: (TargetPt 42 43), structure: structure_road }
--       , { loc: (TargetPt 41 42), structure: structure_road }
--       , { loc: (TargetPt 40 41), structure: structure_road }
--       , { loc: (TargetPt 41 43), structure: structure_road }
--       , { loc: (TargetPt 42 42), structure: structure_road }
--       ]
--   in
--     for_ constructionSites $ \{ loc, structure } -> createConstructionSite room loc structure

-- createConstructionSitesL2 :: Room -> Effect Unit
-- createConstructionSitesL2 room =
--   let 
--     constructionSites =
--       [ { loc: (TargetPt 36 39), structure: structure_extension }
--       , { loc: (TargetPt 29 27), structure: structure_extension }
--       , { loc: (TargetPt 39 41), structure: structure_road }
--       , { loc: (TargetPt 40 40), structure: structure_road }
--       , { loc: (TargetPt 40 42), structure: structure_road }
--       , { loc: (TargetPt 41 41), structure: structure_road }
--       , { loc: (TargetPt 30 28), structure: structure_extension }
--       , { loc: (TargetPt 38 38), structure: structure_extension }
--       , { loc: (TargetPt 40 37), structure: structure_extension }
--       ]
--   in 
--     for_ constructionSites $ \{ loc, structure } -> createConstructionSite room loc structure

-- createConstructionSitesL3 :: Room -> Effect Unit
-- createConstructionSitesL3 room =
--   let 
--     constructionSites =
--       [ { loc: (TargetPt 42 38), structure: structure_extension }
--       , { loc: (TargetPt 38 39), structure: structure_extension }
--       , { loc: (TargetPt 27 21), structure: structure_extension }
--       , { loc: (TargetPt 27 20), structure: structure_extension }
--       , { loc: (TargetPt 39 36), structure: structure_extension }
--       ]
--   in 
--     for_ constructionSites $ \{ loc, structure } -> createConstructionSite room loc structure


spawnNewCreeps :: Spawn -> Int -> Int -> Boolean -> Effect Unit
spawnNewCreeps spawn budget controllerLevel anyHostiles = do
  game <- getGameGlobal
  creepsAndRolesObj <- for (creeps game) $ classifyCreep 

  let 
    creepsAndRoles = fromFoldable creepsAndRolesObj 
    harvesters = creepsAndRoles # mapMaybe (case _ of 
      (Right (Harvester h)) -> Just h
      _ -> Nothing)
    upgraders = creepsAndRoles # mapMaybe (case _ of 
      (Right (Upgrader u)) -> Just u
      _ -> Nothing)
    builders = creepsAndRoles # mapMaybe (case _ of 
      (Right (Builder b)) -> Just b
      _ -> Nothing)

    minHarvesters = 2
    minBuilders = 2
    minUpgraders = 1

  if anyHostiles then spawnGuard
  else if (length harvesters) < minHarvesters then spawnHarvester
  else if (length builders) < minBuilders then spawnBuilder
  else if (length upgraders) < minUpgraders then spawnUpgrader
  else spawnHarvester

  where
    spawnGuard = do
      let plan = constructionPlan Guard.constructionPlans budget
      returnCode <- spawnCreep spawn plan noName { memory: GuardMemory {role: GuardRole}, dryRun: false }
      if returnCode == ok then log $ "Spawned Guard " <> show plan
      else log $ "couldn't create guard with plan: " <> show plan <> ". Error code: " <> show returnCode

    spawnHarvester = do
      let plan = constructionPlan Harvester.constructionPlans budget
      returnCode <- spawnCreep spawn plan noName { memory: HarvesterMemory {role: HarvesterRole, job: Harvester.Harvesting}, dryRun: false }
      if returnCode == ok then log $ "Spawned Harvester " <> show plan
      else log $ "couldn't create harvester with plan: " <> show plan <> ". Error code: " <> show returnCode

    spawnUpgrader = do
      let plan = constructionPlan Upgrader.constructionPlans budget
      returnCode <- spawnCreep spawn plan noName { memory: UpgraderMemory {role: UpgraderRole, job: Upgrader.Harvesting}, dryRun: false }
      if returnCode == ok then log $ "Spawned Upgrader " <> show plan
      else log $ "couldn't create upgrader with plan: " <> show plan <> ". Error code: " <> show returnCode

    spawnBuilder = do
      let plan = constructionPlan Builder.constructionPlans budget
      returnCode <- spawnCreep spawn plan noName { memory: BuilderMemory {role: BuilderRole, job: Builder.Harvesting}, dryRun: false }
      if returnCode == ok then log $ "Spawned Builder " <> show plan
      else log $ "couldn't create builder with plan: " <> show plan <> ". Error code: " <> show returnCode
                

loop :: Effect Unit
loop = do
  game <- getGameGlobal
  let nCreeps = creeps game # size
  
  
  for_ (spawns game) \spawn -> do
    dryRunReturnCode <- spawnCreep spawn [part_move] noName { memory: GuardMemory { role: GuardRole }, dryRun: true }
    
    let 
      canSpawn = dryRunReturnCode == ok
      anyHostiles = find (room spawn) find_hostile_creeps # length # (_ > 0)
      totalCapacity = energyCapacityAvailable (room spawn)
      shouldSpawn = shouldSpawnCreep { nCreeps, totalCapacity } || anyHostiles
      controllerLevel = controller (room spawn) <#> level # fromMaybe 0
      currentCapacity = energyAvailable (room spawn)

    if energyAvailable (room spawn) == totalCapacity && shouldSpawn && canSpawn
    then spawnNewCreeps spawn currentCapacity controllerLevel anyHostiles
    else pure unit

    -- if (constructionSites game # size) == 0 then createConstructionSitesL1 (room spawn) else pure unit
    -- if controllerLevel == 2 && (constructionSites game # size) == 0 then createConstructionSitesL2 (room spawn) else pure unit
    -- if controllerLevel == 3 && (constructionSites game # size) == 0 then createConstructionSitesL3 (room spawn) else pure unit

  for_ (creeps game) \n -> do
    runCreepRole n
    

