module Role.Builder (runBuilder, BuilderMemory, Builder, constructionPlans) where

import Prelude

import CreepRoles (Role)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_sources, part_carry, part_move, part_work, resource_energy)
import Screeps.Creep (amtCarrying, build, carryCapacity, harvestSource, moveTo, say, setAllMemory)
import Screeps.Room (find)
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

type BuilderMemory = { role :: Role, working :: Boolean }
type Builder = { creep :: Creep, mem :: BuilderMemory }

setMemory :: Builder -> BuilderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runBuilder :: Builder -> Effect Unit
runBuilder builder@{ creep, mem } = do

  if mem.working
  then do
    if (creep `amtCarrying` resource_energy) == 0 
    then do
      _ <- say creep "Harvesting"
      setMemory builder (mem { working = false })
    else
      case head (find (room creep) find_construction_sites) of
        Nothing -> creep `say` "I'm stuck" # ignoreM
        Just targetSite -> do
          buildResult <- creep `build` targetSite
          if buildResult == err_not_in_range 
          then creep `moveTo` (TargetObj targetSite) # ignoreM
          else pure unit
  else do
    if (creep `amtCarrying` resource_energy) == (carryCapacity creep)
    then do
      _ <- say creep "working"
      setMemory builder (mem { working = true })
    else do
      case head (find (room creep) find_sources) of
        Nothing -> creep `say` "I'm stuck" # ignoreM
        Just targetSite -> do
          harvest <- creep `harvestSource` targetSite
          if harvest == err_not_in_range 
          then creep `moveTo` (TargetObj targetSite) # ignoreM
          else pure unit
              

