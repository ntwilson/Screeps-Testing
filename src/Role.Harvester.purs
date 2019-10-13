module Role.Harvester 
  (runHarvester
  , Job(..)
  , HarvesterMemory
  , Harvester
  , constructionPlans) where

import Prelude

import CreepRoles (Role)
import Data.Argonaut (class DecodeJson, class EncodeJson, fromString, stringify, toString)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_my_structures, find_sources, part_carry, part_move, part_work, resource_energy, structure_extension, structure_spawn)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, say, setAllMemory, transferToStructure)
import Screeps.Game (getGameGlobal)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Source as Source
import Screeps.Spawn as Spawn
import Screeps.Structure (structureType)
import Screeps.Types (BodyPartType, Creep, RawRoomObject, RawStructure, TargetPosition(..))
import Util (ignoreM)

constructionPlans :: Array (Array BodyPartType)
constructionPlans =
  [ [ part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work]
  , [ part_move, part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work, part_work]
  , [ part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_work, part_work ]
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work ]
  , [ part_move, part_carry, part_carry, part_work ]
  , [ part_move, part_carry, part_work ] 
  ]

data Job = Delivering | Harvesting
type HarvesterMemory = { role :: Role, job :: Job }
type Harvester = { creep :: Creep, mem :: HarvesterMemory }

instance encodeJobJson :: EncodeJson Job where
  encodeJson Delivering = fromString "delivering"
  encodeJson Harvesting = fromString "harvesting"

instance decodeJobJson :: DecodeJson Job where
  decodeJson json 
    | Just "delivering" <- toString json = Right Delivering
    | Just "harvesting" <- toString json = Right Harvesting
    | otherwise = Left $ "Unable to recognize harvester job: " <> stringify json

setMemory :: Harvester -> HarvesterMemory -> Effect Unit
setMemory { creep } mem =  
  setAllMemory creep mem

desiredTarget :: forall a. RawRoomObject (RawStructure a) -> Boolean
desiredTarget struct = 
  (structureType struct) == structure_spawn || (structureType struct) == structure_extension

runHarvester :: Harvester -> Effect Unit
runHarvester harvester@{ creep, mem } =

  case mem.job of
    Delivering -> 
      if creep `amtCarrying` resource_energy == 0
      then do
        _ <- creep `say` "harvesting"
        setMemory harvester (mem { job = Harvesting })
      else do
        game <- getGameGlobal
        let
          myStructures = find (room creep) find_my_structures
          energyHolders = Array.filter (desiredTarget) myStructures
          -- I feel like this shouldn't compile.  This list of structures includes any kind of structure
          -- at all, and we're able to pass it into Spawn.energy which is supposed to only work for spawns.
          -- It should work either way, because Extensions also have an `energy` field, but there's something
          -- wrong with the bindings here. 
          structuresThatCanHoldMore = 
            energyHolders # Array.find (\spawn -> Spawn.energy spawn < Spawn.energyCapacity spawn)
        case structuresThatCanHoldMore of
          Just spawn -> do
            transferResult <- transferToStructure creep spawn resource_energy
            if transferResult == err_not_in_range
            then creep `moveTo` (TargetObj spawn) # ignoreM
            else pure unit
          Nothing -> creep `say` "I'm stuck." # ignoreM
  
    Harvesting ->
      if creep `amtCarrying` resource_energy == carryCapacity creep
      then do
        _ <- creep `say` "delivering"
        setMemory harvester (mem { job = Delivering })
      else
        let sources = find (room creep) find_sources
        in
          case sources # Array.find (Source.energy >>> (_ > 0)) of
            Nothing -> creep `say` "I'm stuck" # ignoreM
            Just targetSource -> do
              harvestResult <- creep `harvestSource` targetSource
              if harvestResult == err_not_in_range
              then creep `moveTo` (TargetObj targetSource) # ignoreM
              else pure unit
          
