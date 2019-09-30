-- | Corresponds to the Screeps API [Flag](http://support.screeps.com/hc/en-us/articles/203079181-Flag)
module Screeps.Flag where

import Effect (Effect)

import Screeps.Types (Color, Flag, ReturnCode, RoomPosition, TargetPosition(..))
import Screeps.FFI (runThisEffFn0, runThisEffFn1, runThisEffFn2, unsafeField)

color :: Flag -> Color
color = unsafeField "color"

memory :: forall a. Flag -> a
memory = unsafeField "memory"

name :: Flag -> String
name = unsafeField "name"

secondaryColor :: Flag -> Color
secondaryColor = unsafeField "secondaryColor"

remove :: forall e. Flag -> Effect ReturnCode
remove = runThisEffFn0 "remove"

setColor :: forall e. Flag -> Color -> Effect ReturnCode
setColor = runThisEffFn1 "setColor"

setColors :: forall e. Flag -> Color -> Color -> Effect ReturnCode
setColors = runThisEffFn2 "setColor"

setPosition :: forall a e. Flag -> TargetPosition a -> Effect ReturnCode
setPosition flag (TargetPt x y) = runThisEffFn2 "setPosition" flag x y
setPosition flag (TargetObj obj) = runThisEffFn1 "setPosition" flag obj
setPosition flag (TargetPos pos) = runThisEffFn1 "setPosition" flag pos
