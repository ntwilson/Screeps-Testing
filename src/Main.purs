module Main (loop) where

import Prelude

import CreepClassification (CreepMemory(..), UnknownCreepType(..), VocationalCreep(..), classifyCreep, spawnCreep)
import CreepRoles (Role(..))
import Data.Array (fromFoldable, length, mapMaybe, zip)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (size)
import Role.Builder (runBuilder)
import Role.Builder as Builder
import Role.Harvester (runHarvester)
import Role.Harvester as Harvester
import Role.Upgrader (runUpgrader)
import Role.Upgrader as Upgrader
import Screeps (structure_extension, structure_road)
import Screeps.Controller (level)
import Screeps.Game (constructionSites, creeps, getGameGlobal, spawns)
import Screeps.Room (controller, createConstructionSite, energyAvailable, energyCapacityAvailable)
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
matchUnit (Left (UnknownCreepType err)) = log $ "One of the creeps has a memory I can't parse.\n" <> err

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = classifyCreep creep >>= matchUnit

energyBudget :: { nCreeps :: Int, totalCapacity :: Int } -> Int
energyBudget { nCreeps, totalCapacity } = 
  min desiredBudget totalCapacity
  where 
    desiredBudget
      | nCreeps < 4 = 200
      | nCreeps < 7 = 300 -- max for room level 1
      | nCreeps < 10 = 400
      | nCreeps < 12 = 500 -- max for room level 2 
      | nCreeps < 15 = 800 -- max for room level 3
      | otherwise = 1000


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
createConstructionSitesL1 :: Room -> Effect Unit
createConstructionSitesL1 room = 
  let 
    constructionSites = 
      [ { loc: (TargetPt 42 43), structure: structure_road }
      , { loc: (TargetPt 41 42), structure: structure_road }
      , { loc: (TargetPt 40 41), structure: structure_road }
      , { loc: (TargetPt 41 43), structure: structure_road }
      , { loc: (TargetPt 42 42), structure: structure_road }
      ]
  in
    for_ constructionSites $ \{ loc, structure } -> createConstructionSite room loc structure

createConstructionSitesL2 :: Room -> Effect Unit
createConstructionSitesL2 room =
  let 
    constructionSites =
      [ { loc: (TargetPt 36 39), structure: structure_extension }
      , { loc: (TargetPt 29 27), structure: structure_extension }
      , { loc: (TargetPt 39 41), structure: structure_road }
      , { loc: (TargetPt 40 40), structure: structure_road }
      , { loc: (TargetPt 40 42), structure: structure_road }
      , { loc: (TargetPt 41 41), structure: structure_road }
      , { loc: (TargetPt 30 28), structure: structure_extension }
      , { loc: (TargetPt 38 38), structure: structure_extension }
      , { loc: (TargetPt 40 37), structure: structure_extension }
      ]
  in 
    for_ constructionSites $ \{ loc, structure } -> createConstructionSite room loc structure


spawnNewCreeps :: Spawn -> Int -> Int -> Effect Unit
spawnNewCreeps spawn budget controllerLevel = do
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

    minHarvesters = 3
    minBuilders = 2
    minUpgraders = 2
    ratios
      | controllerLevel < 2 = { desiredHarvesterRatio: 0.6, desiredBuilderRatio: 0.1, desiredUpgraderRatio: 0.3 }
      | controllerLevel == 2 = { desiredHarvesterRatio: 0.3, desiredBuilderRatio: 0.4, desiredUpgraderRatio: 0.3 }
      | otherwise = { desiredHarvesterRatio: 0.3, desiredBuilderRatio: 0.3, desiredUpgraderRatio: 0.4 }
    { desiredHarvesterRatio, desiredBuilderRatio, desiredUpgraderRatio } = ratios

  if 
    (length harvesters) < minHarvesters  
    || toNumber (length harvesters) / toNumber (length creepsAndRoles) < desiredHarvesterRatio 
  then spawnHarvester
  else if 
    (length upgraders) < minUpgraders  
    || toNumber (length upgraders) / toNumber (length creepsAndRoles) < desiredUpgraderRatio 
  then spawnUpgrader
  else if 
    (length builders) < minBuilders  
    || toNumber (length builders) / toNumber (length creepsAndRoles) < desiredBuilderRatio 
  then spawnBuilder
  else spawnHarvester

  where
    spawnHarvester = do
      let plan = constructionPlan Harvester.constructionPlans budget
      newCreep <- spawnCreep spawn plan noName (HarvesterMemory {role: HarvesterRole, job: Harvester.Harvesting})
      case newCreep of
        Right creep -> log $ "Spawned Harvester " <> show plan <> ": " <> show creep
        Left errCode -> log $ "couldn't create harvester with plan: " <> show plan <> ". Error code: " <> show errCode

    spawnUpgrader = do
      let plan = constructionPlan Upgrader.constructionPlans budget
      newCreep <- spawnCreep spawn plan noName (UpgraderMemory {role: UpgraderRole, job: Upgrader.Harvesting})
      case newCreep of
        Right creep -> log $ "Spawned Upgrader " <> show plan <> ": " <> show creep
        Left errCode -> log $ "couldn't create upgrader with plan: " <> show plan <> ". Error code: " <> show errCode

    spawnBuilder = do
      let plan = constructionPlan Builder.constructionPlans budget
      newCreep <- spawnCreep spawn plan noName (BuilderMemory {role: BuilderRole, job: Builder.Harvesting})
      case newCreep of
        Right creep -> log $ "Spawned Builder " <> show plan <> ": " <> show creep
        Left errCode -> log $ "couldn't create builder with plan: " <> show plan <> ". Error code: " <> show errCode
                

loop :: Effect Unit
loop = do
  game <- getGameGlobal
  let nCreeps = creeps game # size
  
  
  for_ (spawns game) \spawn -> do
    let 
      totalCapacity = energyCapacityAvailable (room spawn)
      budget = energyBudget { nCreeps, totalCapacity }
      controllerLevel = controller (room spawn) <#> level # fromMaybe 0

    if energyAvailable (room spawn) >= budget
    then spawnNewCreeps spawn budget controllerLevel
    else pure unit

    if (constructionSites game # size) == 0 then createConstructionSitesL1 (room spawn) else pure unit
    if controllerLevel == 2 && (constructionSites game # size) == 0 then createConstructionSitesL2 (room spawn) else pure unit

  for_ (creeps game) \n -> do
    runCreepRole n
    

