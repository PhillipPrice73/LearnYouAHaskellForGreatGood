module Geometry.Cube
( volume
, area
) where 

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: FLoat -> Float
area side = Cuboid.area side side side