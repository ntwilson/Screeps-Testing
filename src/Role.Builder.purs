module Role.Builder (runBuilder, BuilderMemory, Builder) where

import Prelude

import Classes (energy, energyCapacity, setMemory)
import CreepRoles (Role)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_sources)
import Screeps.Creep (build, harvestSource, moveTo, say)
import Screeps.Room (find)
import Screeps.RoomObject (room, targetObj)
import Screeps.Types (Creep)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type BuilderMemory = { role :: Role, working :: Boolean }
type Builder = { creep :: Creep, mem :: BuilderMemory }

setBuilderMemory :: Builder -> BuilderMemory -> Effect Unit
setBuilderMemory {creep} mem = setMemory creep mem 

runBuilder :: Builder -> Effect Unit
runBuilder builder@{ creep, mem } = do

  if mem.working
  then do
    if energy creep == 0
    then do
      s <- say creep "Harvesting"
      setBuilderMemory builder (mem { working = false })
    else
      case head (find (room creep) find_construction_sites) of
        Nothing -> 
          pure unit
        Just targetSite -> do
          buildResult <- build creep targetSite
          if buildResult == err_not_in_range 
          then moveTo creep (targetObj targetSite) # ignoreM
          else pure unit
  else 
    if energy creep == energyCapacity creep
    then do
      s <- say creep "working"
      setBuilderMemory builder (mem { working = true })
    else do
      case head (find (room creep) find_sources) of
        Nothing -> do
          pure unit
        Just targetSite -> do
          harvest <- harvestSource creep targetSite
          if harvest == err_not_in_range 
          then moveTo creep (targetObj targetSite) # ignoreM
          else pure unit
              

