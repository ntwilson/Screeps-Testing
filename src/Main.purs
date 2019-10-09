module Main (loop) where

import Prelude

import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (error, logShow)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (filter, isEmpty, size)
import Math (e)
import Record.Builder (Builder)
import Role.Builder (runBuilder)
import Role.Harvester (runHarvester)
import Role.Upgrader (runUpgrader)
import Screeps (find_my_spawns)
import Screeps.Constants (part_carry, part_move, part_work)
import Screeps.Creep (getMemory, setMemory)
import Screeps.Game (creeps, getGameGlobal, spawns)
import Screeps.Memory (toJson)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Spawn (canCreateCreep, createCreep', createCreepInRole)
import Screeps.Types (Creep, CreepRole(..), RawCreep, RawRoomObject, ReturnCode(..), stringToCreepRole)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 


matchUnit :: Creep -> CreepRole -> Effect Unit
matchUnit creep role = do
  case role of
    Harvester -> 
      (runHarvester creep)
    Upgrader -> do
      (runUpgrader creep)
    Builder ->
      (runBuilder creep)
    _ -> do 
      pure unit

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = 
  do
    role <- getMemory creep "role"
    case role of
      Right json -> 
        case (stringToCreepRole json) of
          Just creepRole -> matchUnit creep creepRole
          Nothing -> do pure unit
      Left e ->
        log e

creepToRole :: (RawRoomObject RawCreep) -> Effect (Maybe CreepRole)
creepToRole creep = do
  someRole <- (getMemory creep "role")
  case someRole of
    Left e -> pure Nothing
    Right roleString -> do pure (stringToCreepRole roleString)

creepIsRole :: (RawRoomObject RawCreep) -> CreepRole -> Effect Boolean
creepIsRole creep role = do
  creepRole <- creepToRole creep
  case creepRole of
    Just cr ->
      case role, cr of
        Harvester, Harvester -> do pure true
        Upgrader, Upgrader -> do pure true
        Builder, Builder -> do pure true
        _, _ -> do pure false
    Nothing -> pure false

spawnNewCreeps :: Creep -> Effect Unit
spawnNewCreeps creep =
 let 
   minHarvesters :: Int
   minHarvesters = 5
   minBuilder :: Int
   minBuilder = 2
 in 
    do
      thisGame <- getGameGlobal
      harvesters <- do (pure (filter (\n -> (unsafePerformEffect (creepIsRole n Harvester))) (creeps thisGame))) 
      upgraders <- do (pure (filter (\n -> (unsafePerformEffect (creepIsRole n Upgrader))) (creeps thisGame))) 
      builders <- do (pure (filter (\n -> (unsafePerformEffect (creepIsRole n Builder))) (creeps thisGame))) 
      case (head (find (room creep) find_my_spawns)) of
        Nothing -> pure unit
        Just spawn -> 
          if ((size harvesters) < minHarvesters) then do
            r <- createCreepInRole spawn [part_move, part_work, part_work, part_carry] Harvester
            case r of
              Right e -> logShow e
              Left code -> logShow code
          else
            if ((size builders) < minBuilder) then do
              r <- createCreepInRole spawn [part_move, part_move, part_work, part_carry] Builder
              case r of
                Right e -> logShow e
                Left code -> logShow code
            else do
              r <- createCreepInRole spawn [part_move, part_work, part_work, part_carry] Upgrader
              case r of
                Right e -> logShow e
                Left code -> logShow code


loop :: Effect Unit
loop = do
  game <- getGameGlobal
  if (isEmpty (creeps game)) then
    for_ (spawns game) \spawn -> do
      let x = {role: "\"Harvester\""}
      createCreep' spawn [part_move, part_move, part_work, part_carry] Nothing x
  else  
    pure unit
  for_ (creeps game) \n -> do
    for_ (spawns game) \spawn -> do
      if(canCreateCreep spawn [part_move, part_work, part_work, part_carry] == ReturnCode 0) then spawnNewCreeps n
      else pure unit
    runCreepRole n
    

