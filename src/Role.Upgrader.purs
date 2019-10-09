module Role.Upgrader (runUpgrader) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import CreepRoles (Upgrader)
import Screeps (err_not_in_range, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, upgradeController)
import Screeps.Game (getGameGlobal)
import Screeps.Room (find, controller)
import Screeps.RoomObject (room)
import Screeps.Types (TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

runUpgrader :: Upgrader -> Effect Unit
runUpgrader { creep, mem: { working } } =

  if amtCarrying creep resource_energy < (carryCapacity creep)
  then case head (find (room creep) find_sources) of
    Nothing -> pure unit
    Just targetSource -> do
      harvestResult <- harvestSource creep targetSource
      if harvestResult == err_not_in_range
      then moveTo creep (TargetObj targetSource) # ignoreM
      else pure unit
        
  else do
    game <- getGameGlobal
    case (controller (room creep)) of
      Nothing -> pure unit
      Just controller -> do
        upgradeResult <- upgradeController creep controller
        if upgradeResult == err_not_in_range
        then moveTo creep (TargetObj controller) # ignoreM
        else pure unit