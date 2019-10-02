module Main (loop) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Role.Harvester (run)
import Screeps.Game (creeps, getGameGlobal)

loop :: Effect Unit
loop = do
  game <- getGameGlobal
  for_ (creeps game) run
