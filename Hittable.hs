{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Hittable (
    Hittable, HitRecord, hit, normal,
    pack,
    Sphere
)
where

import Data.List (minimum)

import Raylude
import Vector3 hiding (Vec3)


type HitRecord = (VecType,Hittable,Vector) -- (t,object hit,normal)

-- Hit typeclass and Hittable type, with pack function
class Hit a where
    hit    :: (VecType, VecType) -> Ray -> a -> Maybe HitRecord
    normal :: a -> Point -> Vector

data Hittable = forall a. (Hit a) => Hittable a

instance Hit Hittable where
    hit ts r (Hittable a) = hit ts r a
    normal (Hittable a) = normal a

pack :: (Hit a, Eq a) => a -> Hittable
pack = Hittable


-- Hit instances

-- sphere
type Sphere = (Point,VecType)

instance Hit Sphere where
    hit (tMin,tMax) (o,d) (c,r)
        | qd < 0 = Nothing
        | otherwise = mergeMaybe3ple (closestT, Just (pack ((c,r) :: Sphere)), theNormal)
        where qa = d £. d
              qb = 2 * (d £. (o £- c))
              qc = (o £- c) £. (o £- c) - (r*r)
              qd = qb * qb - 4 * qa * qc
              hitTs = filter (\x -> (x>tMin) && (x<tMax)) [(-qb-sqrt qd) / (2*qa), (-qb+sqrt qd) / (2*qa)]
              closestT = if null hitTs then Nothing else Just (minimum hitTs)
              intersection = ((o,d)@@) <$> closestT
              theNormal = ((£$) (negate . signum)) . (normal (c,r)) <$> intersection
              --theNormal = if theOuterNormal £. d < 0 then theOuterNormal else ((£$) negate) <$> theOuterNormal

    normal (c,_) p = normalise $ p £- c

-- \x -> (x>tMin) && (x<tMax)