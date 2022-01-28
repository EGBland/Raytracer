{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Material (
    Material, scatter,
    pack,
    Diffuse, Specular
)
where

import System.Random (RandomGen)

import Hittable (HitRecord)
import Raylude
import Random
import Vector3

type ScatterRecord = (Colour,Ray) -- attenuation and scatter direction

class Mat a where
    scatter :: (RandomGen g) => a -> g -> Ray -> HitRecord -> (Maybe ScatterRecord,g)

data Material = forall a. Mat a => Material a

instance Mat Material where
    scatter (Material a) = scatter a

pack :: (Mat a, Eq a) => a -> Material
pack = Material

-- Mat instances

-- diffuse
type Diffuse = Colour -- colour is the albedo

instance Mat Diffuse where
    scatter albedo g ray (t,obj,theNormal) =
        let
            intersection = ray @@ t
            (unit,g2) = randomUnit g
            scatterRay = (intersection,theNormal £+ unit)
        in
            (Just (albedo,scatterRay),g2)


type Specular = VecType -- fuzziness

instance Mat Specular where
    scatter fuzz g (o,d) (t,obj,theNormal) =
        let
            intersection = (o,d) @@ t
            (unit,g2) = randomUnit g
            direction = normalise $ (d £- (2 * (d £. theNormal) £* theNormal)) £+ fuzz £* unit
        in
            if direction £. theNormal <= 0 then (Nothing,g2) else (Just ((1,1,1),(intersection,direction)),g2)