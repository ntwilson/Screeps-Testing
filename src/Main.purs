module Main (loop) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Foreign.Object (filter, isEmpty, size)
import Role.Builder (runBuilder)
import Role.Harvester (runHarvester)
import Role.Upgrader (runUpgrader)
import Screeps.Constants (part_carry, part_move, part_work)
import Screeps.Creep (getMemory)
import Screeps.Game (creeps, getGameGlobal, spawns)
import Screeps.Spawn (canCreateCreep, createCreep')
import Screeps.Types (Creep, RawCreep, RawRoomObject, ReturnCode(..), Spawn)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

noName :: Maybe String 
noName = Nothing

matchUnit :: Creep -> String -> Effect Unit
matchUnit creep "Harvester" = runHarvester creep
matchUnit creep "Upgrader" = runUpgrader creep
matchUnit creep "Builder" = runBuilder creep
matchUnit _ _ = pure unit

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = 
  do
    role <- getMemory creep "role"
    case role of
      Right json -> 
        matchUnit creep json
      Left e ->
        log e

creepToRole :: (RawRoomObject RawCreep) -> Effect String
creepToRole creep = do
  role <- (getMemory creep "role")
  case role of
    Left e -> do
      pure "undefined"
    Right r ->
      pure r

creepIsRole :: (RawRoomObject RawCreep) -> String -> Effect Boolean
creepIsRole creep role = do
  creepRole <- creepToRole creep
  pure (role == creepRole)

spawnNewCreeps :: Spawn -> Effect Unit
spawnNewCreeps spawn =
 let 
   minHarvesters = 5
   minBuilder = 2
 in 
    do
      thisGame <- getGameGlobal
      creepsAndRoles <- 
        for (creeps thisGame) $ \crp -> do 
          role <- creepToRole crp
          pure { creep: crp, role }

      let
        harvesters = creepsAndRoles # filter (\{role} -> role == "Harvester") # map (_.creep)
        upgraders = creepsAndRoles # filter (\{role} -> role == "Upgrader") # map (_.creep) 
        builders = creepsAndRoles # filter (\{role} -> role == "Builder") # map (_.creep) 

      if ((size harvesters) < minHarvesters) then do
        let x ={role: "\"Harvester\""}
        createCreep' spawn [part_move, part_work, part_work, part_carry] noName x >>= logShow
      else if ((size builders) < minBuilder) then do
        let x = {role: "\"Builder\"", working: "\"true\""}
        createCreep' spawn [part_move, part_move, part_work, part_carry] noName x >>= logShow
      else do
        let x = {role: "\"Upgrader\"", working: "\"true\""}
        createCreep' spawn [part_move, part_work, part_work, part_carry] noName x >>= logShow
            

loop :: Effect Unit
loop = do
  game <- getGameGlobal
  if (isEmpty (creeps game)) then
    for_ (spawns game) \spawn ->
      createCreep' spawn [part_move, part_move, part_work, part_carry] noName {role: "\"Harvester\""}
  else  
    pure unit

  for_ (spawns game) \spawn -> do
    if canCreateCreep spawn [part_move, part_work, part_work, part_carry] == ReturnCode 0 
    then spawnNewCreeps spawn
    else pure unit

  for_ (creeps game) \n -> do
    runCreepRole n
    

