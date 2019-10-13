module Role.Builder 
  ( runBuilder
  , Job(..)
  , BuilderMemory
  , Builder
  , constructionPlans) where

import Prelude

import CreepRoles (Role)
import Data.Argonaut (class DecodeJson, class EncodeJson, fromString, stringify, toString)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_sources_active, part_carry, part_move, part_work, resource_energy)
import Screeps.Creep (amtCarrying, build, carryCapacity, harvestSource, moveTo, say, setAllMemory)
import Screeps.Room (find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByPath)
import Screeps.Types (BodyPartType, Creep, FindContext(..), TargetPosition(..))
import Util (ignoreM)


constructionPlans :: Array (Array BodyPartType)
constructionPlans =
  [ [ part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work]
  , [ part_move, part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work, part_work]
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work, part_work, part_work ]
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ]
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work ]
  , [ part_move, part_move, part_carry, part_work, part_work ]
  , [ part_move, part_carry, part_work, part_work ]
  , [ part_move, part_carry, part_work ] 
  ]

data Job = Building | Harvesting
type BuilderMemory = { role :: Role, job :: Job }
type Builder = { creep :: Creep, mem :: BuilderMemory }

instance encodeJobJson :: EncodeJson Job where
  encodeJson Building   = fromString "building"
  encodeJson Harvesting = fromString "harvesting"

instance decodeJobJson :: DecodeJson Job where
  decodeJson json 
    | Just "building" <- toString json   = Right Building
    | Just "harvesting" <- toString json = Right Harvesting 
    | otherwise = Left $ "Unable to recognize builder job: " <> stringify json

setMemory :: Builder -> BuilderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runBuilder :: Builder -> Effect Unit
runBuilder builder@{ creep, mem } = do

  case mem.job of
    Building ->
      if creep `amtCarrying` resource_energy == 0 
      then do
        _ <- say creep "harvesting"
        setMemory builder (mem { job = Harvesting })
      else
        case head (find (room creep) find_construction_sites) of
          Nothing -> creep `say` "I'm stuck" # ignoreM
          Just targetSite -> do
            buildResult <- creep `build` targetSite
            if buildResult == err_not_in_range 
            then creep `moveTo` (TargetObj targetSite) # ignoreM
            else pure unit


    Harvesting ->
      if creep `amtCarrying` resource_energy == carryCapacity creep
      then do
        _ <- say creep "building"
        setMemory builder (mem { job = Building })
      else do
        closestSource <- findClosestByPath (pos creep) (OfType find_sources_active) 
        case closestSource of
          Nothing -> creep `say` "I'm stuck" # ignoreM
          Just targetSite -> do
            harvest <- creep `harvestSource` targetSite
            if harvest == err_not_in_range 
            then creep `moveTo` (TargetObj targetSite) # ignoreM
            else pure unit
                

