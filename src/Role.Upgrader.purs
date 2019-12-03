module Role.Upgrader (runUpgrader, UpgraderMemory, Upgrader) where

import Prelude

import Classes (energy, energyCapacity, setMemory)
import CreepRoles (Role)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_sources)
import Screeps.Creep (harvestSource, moveTo, say, upgradeController)
import Screeps.Game (getGameGlobal)
import Screeps.Room (find, controller)
import Screeps.RoomObject (room, targetObj)
import Screeps.Types (Creep)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 


type UpgraderMemory = { role :: Role, working :: Boolean }
type Upgrader = { creep :: Creep, mem :: UpgraderMemory }

setUpgraderMemory :: Upgrader -> UpgraderMemory -> Effect Unit
setUpgraderMemory {creep} mem = setMemory creep mem 

runUpgrader :: Upgrader -> Effect Unit
runUpgrader upgrader@{ creep, mem } =

  if mem.working
  then
    if energy creep == 0
    then do
      s <- say creep "Harvesting"
      setUpgraderMemory upgrader (mem { working = false })
    else do
      game <- getGameGlobal
      case (controller (room creep)) of
        Nothing -> pure unit
        Just controller -> do
          upgradeResult <- upgradeController creep controller
          if upgradeResult == err_not_in_range
          then moveTo creep (targetObj controller) # ignoreM
          else pure unit

  else 
    if energy creep == energyCapacity creep
    then do
      s <- say creep "Upgrading"
      setUpgraderMemory upgrader (mem { working = true }) 
    else
      case head (find (room creep) find_sources) of
        Nothing -> pure unit
        Just targetSource -> do
          harvestResult <- harvestSource creep targetSource
          if harvestResult == err_not_in_range
          then moveTo creep (targetObj targetSource) # ignoreM
          else pure unit
        
