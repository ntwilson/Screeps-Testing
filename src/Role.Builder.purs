module Role.Builder (runBuilder) where

import Prelude

import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (overF)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (lookup)
import Math (e)
import Screeps (err_not_in_range, find_construction_sites, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, build, carryCapacity, getMemory, harvestSource, moveTo, say, setMemory, transferToStructure)
import Screeps.Creep (upgradeController, getMemory, setMemory)
import Screeps.Game (getGameGlobal, spawns)
import Screeps.Room (find, controller)
import Screeps.RoomObject (room)
import Screeps.Types (Creep, ResourceType(..), TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

runBuilder :: Creep -> Effect Unit
runBuilder creep = do

  creepIsBuilding <- getMemory creep "working"
  case creepIsBuilding of
    Left e -> 
      do setMemory creep "working" "\"false\""
    Right isBuilding ->
      case isBuilding of
        "true" -> 
          case ((amtCarrying creep (ResourceType "energy")) == 0) of
            true -> 
              do
                s <- say creep "Harvesting"
                setMemory creep "working" "\"false\""
            false ->
              case head (find (room creep) find_construction_sites) of
                Nothing -> do
                  pure unit
                Just targetSite -> do
                  buildResult <- build creep targetSite
                  if buildResult == err_not_in_range 
                  then moveTo creep (TargetObj targetSite) # ignoreM
                  else pure unit
        "false" -> 
          case ((amtCarrying creep (ResourceType "energy")) == (carryCapacity creep)) of
            true -> do
              s <- say creep "working"
              setMemory creep "working" "\"true\""
            false ->
              case head (find (room creep) find_sources) of
                Nothing -> do
                  pure unit
                Just targetSite -> do
                  harvest <- harvestSource creep targetSite
                  if harvest == err_not_in_range 
                  then moveTo creep (TargetObj targetSite) # ignoreM
                  else pure unit
        _ -> do
          pure unit
              

      