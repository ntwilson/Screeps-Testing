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
import Screeps.Spawn (canCreateCreep, createCreep')
import Screeps.Types (Creep, RawCreep, RawRoomObject)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 


matchUnit :: Creep -> String -> Effect Unit
matchUnit creep role = do
  case role of
    "Harvester" -> 
      (runHarvester creep)
    "Upgrader" -> 
      (runUpgrader creep)
    "Builder" ->
      (runBuilder creep)
    _ -> pure unit

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = 
  do
    role <- getMemory creep "\"role\""
    case role of
      Right json -> 
        matchUnit creep json
      Left e ->
        log e

creepToRole :: (RawRoomObject RawCreep) -> Effect String
creepToRole creep = do
  role <- (getMemory creep "\"role\"")
  case role of
    Left e -> do
      pure "undefined"
    Right role ->
      pure role

creepIsRole :: (RawRoomObject RawCreep) -> String -> Effect Boolean
creepIsRole creep role = do
  creepRole <- creepToRole creep
  pure (role == creepRole)

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
      harvesters <- do (pure (filter (\n -> (unsafePerformEffect (creepIsRole n "Harvester"))) (creeps thisGame))) 
      upgraders <- do (pure (filter (\n -> (unsafePerformEffect (creepIsRole n "Upgrader"))) (creeps thisGame))) 
      builders <- do (pure (filter (\n -> (unsafePerformEffect (creepIsRole n "Builder"))) (creeps thisGame))) 
      case (head (find (room creep) find_my_spawns)) of
        Nothing -> pure unit
        Just spawn -> 
          if ((size harvesters) < minHarvesters) then do
            r <- createCreep' spawn [part_move, part_work, part_work, part_carry] Nothing "{\"role\":\"Harvester\"}"
            case r of
              Right e -> logShow e
              Left code -> logShow code
          else
            if ((size builders) < minBuilder) then do
              r <- createCreep' spawn [part_move, part_move, part_work, part_carry] Nothing "{\"role\":\"Builder\", \"Building\":\"false\"}"
              case r of
                Right e -> logShow e
                Left code -> logShow code
            else do
              r <- createCreep' spawn [part_move, part_work, part_work, part_carry] Nothing "{{\"memory\": \"role\":\"Upgrader\"}}"
              case r of
                Right e -> logShow e
                Left code -> logShow code


loop :: Effect Unit
loop = do
  game <- getGameGlobal
  if (isEmpty (creeps game)) then
    for_ (spawns game) \spawn -> do
      createCreep' spawn [part_move, part_move, part_work, part_carry] Nothing "{{\"memory\": \"role\":\"Upgrader\"}}"
  else  
    pure unit
  for_ (creeps game) \n -> do
    spawnNewCreeps n
    runCreepRole n
    

