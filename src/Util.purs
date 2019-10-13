module Util (map2D, (<<$>>), map2DFlipped, (<<#>>), bodyPartCost, ignore, ignoreM) where

import Prelude

import Screeps (bodypart_cost, part_attack, part_carry, part_heal, part_move, part_ranged_attack, part_tough, part_work)
import Screeps.Types (BodyPartType)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

bodyPartCost :: BodyPartType -> Int
bodyPartCost part
  | part == part_move = bodypart_cost.move
  | part == part_work = bodypart_cost.work
  | part == part_carry = bodypart_cost.carry
  | part == part_tough = bodypart_cost.tough
  | part == part_heal = bodypart_cost.heal
  | part == part_attack = bodypart_cost.attack
  | part == part_ranged_attack = bodypart_cost.ranged_attack
  | otherwise = 0

map2D :: forall f1 f2 a b. Functor f1 => Functor f2 => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
map2D f xs = map (map f) xs
infixr 1 map2D as <<$>>

map2DFlipped :: forall f1 f2 a b. Functor f1 => Functor f2 => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
map2DFlipped = flip map2D
infixl 1 map2DFlipped as <<#>>

