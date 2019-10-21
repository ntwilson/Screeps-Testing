module Role.Harvester 
  (runHarvester
  , Job(..)
  , HarvesterMemory
  , Harvester
  , constructionPlans) where

import Prelude

import CreepRoles (Role)
import CreepTasks (deliverToClosestStructure, harvestEnergy)
import Data.Argonaut (class DecodeJson, class EncodeJson, fromString, stringify, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (part_carry, part_move, part_work, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, say, setAllMemory)
import Screeps.Types (BodyPartType, Creep)

constructionPlans :: Array (Array BodyPartType)
constructionPlans =
  [ [ part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work] -- 1000 (14 extensions)
  , [ part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work] -- 800 (10 extensions)
  , [ part_move, part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work, part_work] -- 600
  , [ part_move, part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ] -- 550 (5 extensions)
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ] -- 500
  , [ part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work ] -- 450
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work ] -- 400
  , [ part_move, part_move, part_carry, part_work, part_work ] -- 350
  , [ part_move, part_carry, part_work, part_work ] -- 300 (just the spawn)
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

runHarvester :: Harvester -> Effect Unit
runHarvester harvester@{ creep, mem } =

  case mem.job of
    Delivering -> 
      if creep `amtCarrying` resource_energy == 0
      then do
        _ <- creep `say` "harvesting"
        setMemory harvester (mem { job = Harvesting })
      else deliverToClosestStructure creep
  
    Harvesting ->
      if creep `amtCarrying` resource_energy == carryCapacity creep
      then do
        _ <- creep `say` "delivering"
        setMemory harvester (mem { job = Delivering })
      else harvestEnergy creep
          
