{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Hittable (
    pack,
    Hittable, HitRecord, hits, normal,
    Sphere, Plane,
    Vec3, Ray, VecType
)
where

import Raylude
import qualified Vector3
import Vector3 ( (£+), (£-), (£*), (£.), (££), (£$), (@@), norm, normalise, gradient, vec3x, vec3y, vec3z, vmean, isZero )
import SimEq

type Sphere = (Vec3, VecType)
type Plane = (Vec3, Vec3, Vec3)

type Point = Vec3
type ParamT = VecType
type Normal = Vec3
type HitRecord = (ParamT,Hittable,Normal)

class Hit a where
    hits   :: Ray -> a -> Maybe HitRecord
    normal :: a -> Point -> Normal

data Hittable = forall a. (Hit a) => Hittable a

instance Hit Hittable where
    x `hits` (Hittable a) = x `hits` a
    normal (Hittable a) = normal a

pack :: (Hit a, Eq a) => a -> Hittable
pack = Hittable


-- utility fn
mergeMaybe3ple :: (Maybe a, Maybe b, Maybe c) -> Maybe (a,b,c)
mergeMaybe3ple (a,b,c) = do
    a' <- a
    b' <- b
    c' <- c
    return (a',b',c')

-- instances

instance Hit Sphere where
    (origin,direction) `hits` sph
        | discriminant < 0 = Nothing
        | otherwise = mergeMaybe3ple (closestT, Just (pack (sph :: Sphere)), theNormal)
        where (centre,radius) = sph
              a = direction £. direction
              b = 2 * (direction £. (origin £- centre))
              c = (origin £- centre) £. (origin £- centre) - (radius*radius)
              discriminant = b*b - 4*a*c
              hitTs = filter (>0.001) $ [(-b - sqrt discriminant) / (2*a), (-b + sqrt discriminant) / (2*a)]
              closestT = if hitTs == [] then Nothing else Just (minimum hitTs)
              theNormal = ((normal (centre,radius)) . ((origin,direction)@@)) <$> closestT
    
    normal (c,_) p = normalise (p £- c)

instance Hit Plane where
    ((ox,oy,oz),(dx,dy,dz)) `hits` plane
        | isZero (a,b,c) = Nothing
        | otherwise = if t < 0.001 then Nothing else Just (t, pack (plane :: Plane), (a,b,c))
        where (p1,p2,p3) = plane
              (a,b,c) = normal (p1,p2,p3) (0,0,0)
              d = (a,b,c) `eval` p1
              t = (a*ox + b*oy + c*oz + d) / (a*dx + b*dy + c*dz)
    
    normal (p1,p2,p3) _ = normalise $ (p1 £- p2) ££ (p1 £- p3)