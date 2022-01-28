module Random (
    uniformRs, randomUnit
)
where

import System.Random

import Raylude

uniformRs :: (RandomGen g, UniformRange a) => Int -> (a,a) -> g -> ([a],g)
uniformRs n range g
    | n < 0     = error "Negative length list"
    | n == 0    = ([],g)
    | otherwise = foldr (\_ (xs,g) -> let (r,g2) = uniformR range g in (r:xs,g2)) ([],g) [1..n]
    | otherwise = let (r,g2) = uniformR range g
                      (lst,g3) = uniformRs (n-1) range g2
                      in (r:lst,g3)

randomUnitPolar :: (RandomGen g) => g -> (Vector,g)
randomUnitPolar g = let (theta,g2) = uniformR (0::VecType,2*pi::VecType) g
                        (phi,  g3) = uniformR (0::VecType,2*pi::VecType) g2
                        sintheta   = sin theta
                        costheta   = cos theta
                        sinphi     = sin phi
                        cosphi     = cos phi -- idk if these cache
                        in ((sintheta*cosphi, sintheta*sinphi, costheta),g3)

randomUnit :: (RandomGen g) => g -> (Vector,g)
randomUnit = randomUnitPolar