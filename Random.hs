module Random (
    uniformRs, randomUnit
)
where

import Raylude
import System.Random (RandomGen, UniformRange, uniformR)
import Vector3 ((£*), normalise, norm)

uniformRs :: (RandomGen g, UniformRange a) => Int -> (a,a) -> g -> ([a],g)
uniformRs n range g
    | n < 0     = error "Negative length list"
    | n == 0    = ([],g)
    | otherwise = foldr (\_ (xs,g) -> let (r,g2) = uniformR range g in (r:xs,g2)) ([],g) [1..n]

randomUnitDefault :: (RandomGen g) => g -> (Vec3,g)
randomUnitDefault g = let (rs,g2) = uniformRs 3 (-1::VecType,1::VecType) g
                          (r,g3)  = uniformR (0.001::VecType,0.999::VecType) g2
                          in (r £* normalise (rs!!0,rs!!1,rs!!2),g3)

randomUnitPolar :: (RandomGen g) => g -> (Vec3,g)
randomUnitPolar g = let (theta,g2) = uniformR ( 0::VecType,2*pi::VecType) g
                        (phi,  g3) = uniformR ( 0::VecType,2*pi::VecType) g2
                        sintheta   = sin theta
                        costheta   = cos theta
                        sinphi     = sin phi
                        cosphi     = cos phi -- idk if these cache
                        in ((sintheta*cosphi, sintheta*sinphi, costheta),g3)

randomUnitRejection :: (RandomGen g) => g -> (Vec3,g)
randomUnitRejection g = let (rs,g2) = uniformRs 3 (-1::VecType,1::VecType) g
                            vec = (rs!!0,rs!!1,rs!!2)
                            in if norm vec < 1 then (vec,g2) else randomUnit g2

randomUnit :: (RandomGen g) => g -> (Vec3,g)
randomUnit = randomUnitPolar