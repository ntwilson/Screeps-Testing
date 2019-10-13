module Role.Upgrader 
  ( runUpgrader
  , Job(..)
  , UpgraderMemory
  , Upgrader
  , constructionPlans) where

import Prelude

import CreepRoles (Role)
import Data.Argonaut (class DecodeJson, class EncodeJson, fromString, stringify, toString)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_sources, part_carry, part_move, part_work, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, say, setAllMemory, upgradeController)
import Screeps.Game (getGameGlobal)
import Screeps.Room (find, controller)
import Screeps.RoomObject (room)
import Screeps.Types (BodyPartType, Creep, TargetPosition(..))
import Util (ignoreM)

constructionPlans :: Array (Array BodyPartType)
constructionPlans =
  [ [ part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work]
  , [ part_move, part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work, part_work]
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work, part_work, part_work ]
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work ]
  , [ part_move, part_carry, part_work, part_work ]
  , [ part_move, part_carry, part_work ] 
  ]

data Job = Upgrading | Harvesting
type UpgraderMemory = { role :: Role, job :: Job }
type Upgrader = { creep :: Creep, mem :: UpgraderMemory }

instance encodeJobJson :: EncodeJson Job where
  encodeJson Upgrading  = fromString "upgrading"
  encodeJson Harvesting = fromString "harvesting"

instance decodeJobJson :: DecodeJson Job where
  decodeJson json 
    | Just "upgrading" <- toString json  = Right Upgrading
    | Just "harvesting" <- toString json = Right Harvesting 
    | otherwise = Left $ "Unable to recognize upgrader job: " <> stringify json

setMemory :: Upgrader -> UpgraderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runUpgrader :: Upgrader -> Effect Unit
runUpgrader upgrader@{ creep, mem } =

  case mem.job of
    Upgrading ->
      if amtCarrying creep resource_energy == 0
      then do
        _ <- creep `say` "harvesting"
        setMemory upgrader (mem { job = Harvesting })
      else do
        game <- getGameGlobal
        case (controller (room creep)) of
          Nothing -> creep `say` "I'm stuck" # ignoreM
          Just controller -> do
            upgradeResult <- creep `upgradeController` controller
            if upgradeResult == err_not_in_range
            then creep `moveTo` (TargetObj controller) # ignoreM
            else pure unit

    Harvesting -> 
      if creep `amtCarrying` resource_energy == carryCapacity creep
      then do
        _ <- creep `say` "upgrading"
        setMemory upgrader (mem { job = Upgrading }) 
      else
        case head (find (room creep) find_sources) of
          Nothing -> creep `say` "I'm stuck" # ignoreM
          Just targetSource -> do
            harvestResult <- creep `harvestSource` targetSource
            if harvestResult == err_not_in_range
            then creep `moveTo` (TargetObj targetSource) # ignoreM
            else pure unit
          
