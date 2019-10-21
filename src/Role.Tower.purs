module Role.Tower (runTower) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (find_hostile_creeps)
import Screeps.RoomObject (pos)
import Screeps.RoomPosition (findClosestByRange)
import Screeps.Tower (attack)
import Screeps.Types (FindContext(..), Tower)
import Util (ignoreM)

runTower :: Tower -> Effect Unit
runTower tower = do 
  findResult <- (pos tower) `findClosestByRange` (OfType find_hostile_creeps)
  case findResult of 
    Just hostile -> tower `attack` hostile # ignoreM
    Nothing -> pure unit

