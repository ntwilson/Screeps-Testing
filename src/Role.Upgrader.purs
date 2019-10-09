module Role.Upgrader (runUpgrader) where

import Prelude

import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (lookup)
import Screeps (err_not_in_range, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, getMemory, harvestSource, moveTo, setMemory, transferToStructure)
import Screeps.Creep (upgradeController)
import Screeps.Game (getGameGlobal, spawns)
import Screeps.Room (find, controller)
import Screeps.RoomObject (room)
import Screeps.Types (Creep, ResourceType(..), TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

runUpgrader :: Creep -> Effect Unit
runUpgrader creep = do
  isWorking <- getMemory creep "working"
  case isWorking of
    Left e -> do setMemory creep "working" "\"false\""
    Right working -> 
      case working of
        "false" ->
          if (amtCarrying creep (ResourceType "energy") < (carryCapacity creep))
          then 
            case head (find (room creep) find_sources) of
              Nothing -> do 
                pure unit
              Just targetSource -> do
                harvestResult <- harvestSource creep targetSource
                if harvestResult == err_not_in_range
                then moveTo creep (TargetObj targetSource) # ignoreM
                else pure unit
          else do 
            setMemory creep "working" "\"true\""
        "true" -> do
          game <- getGameGlobal
          if (amtCarrying creep (ResourceType "energy") > 0) then
            case (controller (room creep)) of
              Nothing -> do
                pure unit
              Just controller -> do
                upgradeResult <- upgradeController creep controller
                if upgradeResult == err_not_in_range
                then do
                  moveTo creep (TargetObj controller) # ignoreM
                else do 
                  pure unit
          else do 
            setMemory creep "working" "\"false\""
        _ -> setMemory creep "working" "\"false\""