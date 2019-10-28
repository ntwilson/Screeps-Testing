module Role.Tower (runTower) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (find_hostile_creeps, find_my_creeps)
import Screeps.Creep (hits, hitsMax)
import Screeps.RoomObject (pos)
import Screeps.RoomPosition (findClosestByRange, findClosestByRange')
import Screeps.Tower (attack, heal)
import Screeps.Types (FindContext(..), Tower, Creep)
import Util (ignoreM)

runTower :: Tower -> Effect Unit
runTower tower = do 
  findResult <- (pos tower) `findClosestByRange` (OfType find_hostile_creeps)
  case findResult of 
    Just hostile -> tower `attack` hostile # ignoreM
    Nothing -> do
      findResult2 <- findClosestByRange' (pos tower) (OfType find_my_creeps) wounded
      case findResult2 of 
        Just woundedCreep -> tower `heal` woundedCreep # ignoreM
        Nothing -> pure unit

  where 
    wounded :: Creep -> Boolean
    wounded creep = hits creep < hitsMax creep 
