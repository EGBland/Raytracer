module Raylude (
    Vec3, Ray, VecType, Pixel
)
where

import qualified Vector3

type VecType = Float
type Vec3 = Vector3.Vec3 VecType
type Ray = Vector3.Ray3 VecType
type Pixel = Vector3.Vec3 Int