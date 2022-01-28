{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Hittable (
    Hittable, HitRecord, hit, normal,
    pack,
    Sphere, Plane
)
where

import Data.List (minimum)
import Data.Maybe (fromJust, isNothing)

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
              theNormal = (\p -> let n = normal (c,r) p in if n £. d < 0 then n else negate £$ n) <$> intersection
              --theNormal = if theOuterNormal £. d < 0 then theOuterNormal else ((£$) negate) <$> theOuterNormal

    normal (c,_) p = normalise $ p £- c


-- plane
type Plane = (Point,Point,Point)

instance Hit Plane where
    hit (tMin,tMax) ((ox,oy,oz),(dx,dy,dz)) (p1,p2,p3)
        | (dx,dy,dz) £. (a,b,c) == 0 = Nothing
        | (t < tMin) || (t > tMax) = Nothing
        | otherwise = Just (t, pack ((p1,p2,p3)::Plane), theNormal)
        where 
            (x,y,z) = p1
            (a,b,c) = (p1 £- p2) ££ (p1 £- p3)
            d       = a*x + b*y + c*z
            t       = (a*ox + b*oy + c*oz + d) / (a*dx + b*dy + c*dz)
            theNormal = if (a,b,c) £. (dx,dy,dz) < 0 then (a,b,c) else (-a,-b,-c)
    
    normal (p1,p2,p3) _ = (p1 £- p2) ££ (p1 £- p3)


-- disk
type Disk = (Plane,Point,VecType) -- plane, centre, radius

instance Hit Disk where
    hit (tMin,tMax) (o,dir) (plane,centre,radius)
        | isNothing planeHit = Nothing
        | isNothing distance = Nothing
        | otherwise = let d = fromJust distance in if d > radius then Nothing else mergeMaybe3ple (fst3 <$> planeHit, Just (pack ((plane,centre,radius)::Disk)), Just theNormal)
        where planeHit = hit (tMin,tMax) (o,dir) plane
              intersection = (((o,dir) @@) . fst3) <$> planeHit
              distance = (veclen . (£- centre)) <$> intersection
              theNormal = let n = normal plane (0,0,0) in if n £. dir < 0 then n else negate £$ n

    normal (plane,_,_) = normal plane