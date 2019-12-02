module Role.Tower (runTower) where

import Prelude

import Classes (hits, hitsMax)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (find_hostile_creeps, find_my_creeps)
import Screeps.RoomObject (pos)
import Screeps.RoomPosition (findClosestByRange, findClosestByRange')
import Screeps.Tower (attack, heal)
import Screeps.Types (Creep, Tower)
import Util (ignoreM)

runTower :: Tower -> Effect Unit
runTower tower = do 
  findResult <- (pos tower) `findClosestByRange` find_hostile_creeps
  case findResult of 
    Just hostile -> tower `attack` hostile # ignoreM
    Nothing -> do
      findResult2 <- findClosestByRange' (pos tower) find_my_creeps wounded
      case findResult2 of 
        Just woundedCreep -> tower `heal` woundedCreep # ignoreM
        Nothing -> pure unit

  where 
    wounded :: Creep -> Boolean
    wounded creep = hits creep < hitsMax creep 
