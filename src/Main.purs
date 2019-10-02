module Main (loop) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Role.Harvester (runHarvester)
import Role.Upgrader (runUpgrader)
import Screeps.Types (Creep)
import Screeps.Game (creeps, getGameGlobal)
import Screeps.Creep (getMemory)
import Data.Either (either, fromRight, isRight)
import Partial

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

returnUnit :: forall a. a -> Effect Unit
returnUnit a = pure unit

matchUnit :: Creep -> String -> Effect Unit
matchUnit creep role = 
  if role == "harvester"
    then
      (runHarvester creep)
    else 
      (runUpgrader creep)

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = 
  do
    role <- getMemory creep "role"
    either returnUnit (matchUnit creep) role

loop :: Effect Unit
loop = do
  game <- getGameGlobal
  for_ (creeps game) runCreepRole
    

