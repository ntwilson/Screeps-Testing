module Role.Upgrader (runUpgrader, UpgraderMemory, Upgrader, constructionPlans) where

import Prelude

import CreepRoles (Role)
import Data.Array (head)
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
  , [ part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_work, part_work ]
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work ]
  , [ part_move, part_carry, part_carry, part_work ]
  , [ part_move, part_carry, part_work ] 
  ]

type UpgraderMemory = { role :: Role, working :: Boolean }
type Upgrader = { creep :: Creep, mem :: UpgraderMemory }

setMemory :: Upgrader -> UpgraderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runUpgrader :: Upgrader -> Effect Unit
runUpgrader upgrader@{ creep, mem } =

  if mem.working
  then
    if amtCarrying creep resource_energy == 0
    then do
      _ <- creep `say` "Harvesting"
      setMemory upgrader (mem { working = false })
    else do
      game <- getGameGlobal
      case (controller (room creep)) of
        Nothing -> creep `say` "I'm stuck" # ignoreM
        Just controller -> do
          upgradeResult <- creep `upgradeController` controller
          if upgradeResult == err_not_in_range
          then creep `moveTo` (TargetObj controller) # ignoreM
          else pure unit

  else 
    if creep `amtCarrying` resource_energy == carryCapacity creep
    then do
      _ <- creep `say` "Upgrading"
      setMemory upgrader (mem { working = true }) 
    else
      case head (find (room creep) find_sources) of
        Nothing -> creep `say` "I'm stuck" # ignoreM
        Just targetSource -> do
          harvestResult <- creep `harvestSource` targetSource
          if harvestResult == err_not_in_range
          then creep `moveTo` (TargetObj targetSource) # ignoreM
          else pure unit
        
