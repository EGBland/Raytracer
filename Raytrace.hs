{-# LANGUAGE FlexibleInstances #-}

import Control.Monad (sequence_)
import Data.List (minimum, minimumBy)
import Data.Maybe (isNothing, fromJust)
import Data.Ord (comparing)
import System.Random
import Data.UnixTime
import Text.Printf

import Raylude
import qualified Vector3
import Vector3 ( (£+), (£-), (£*), (£.), (££), (£$), (@@), norm, normalise, gradient, vec3x, vec3y, vec3z, vmean )
import Hittable
import Random

type World = [Hittable]


-- utility stuff
mergeMaybe2ple :: (Maybe a, Maybe b) -> Maybe (a,b)
mergeMaybe2ple (a,b) = do
    a' <- a
    b' <- b
    return (a',b')

clamp :: (Ord a) => a -> a -> a -> a
clamp theMin theMax x
    | x < theMin = theMin
    | x > theMax = theMax
    | otherwise  = x

-- image stuff
createP3 :: Int -> Int -> [Pixel] -> String
createP3 width height pixels = (printf "P3\n%d\t%d\t255\n" width height) ++ (concat . (map writePixel) $ pixels)

writePixel :: Pixel -> String
writePixel (r,g,b) = printf "%d\t%d\t%d" r g b

gammaCorrect :: Vec3 -> Pixel
gammaCorrect = (£$) $ floor . (*255) . sqrt . (clamp 0 1)

colourFuncDiffuse :: World -> Ray -> [Vec3] -> Pixel
colourFuncDiffuse world ray nextRays = gammaCorrect $ colourFuncDiffuse' world ray nextRays

colourFuncDiffuse' :: World -> Ray -> [Vec3] -> Vec3
colourFuncDiffuse' world (o,d) [] = (0,0,0)
colourFuncDiffuse' world (o,d) (nextRay:rays)
    | isNothing closestHit = let t = vec3y d in gradient (0.5, 0.7, 1.0) (1.0,1.0,1.0) (0.5 * (t + 1.0))
    | otherwise = let (chT,chObj,theNormal) = fromJust closestHit
                      intersection = (o,d) @@ chT
                      diffuseRay = (intersection, normalise $ nextRay £+ theNormal)
                      nextColour = colourFuncDiffuse' world diffuseRay rays
                      in 0.5 £* nextColour
    where theHits = filter (not . isNothing) . map (hits (o,d)) $ world
          closestHit = if null theHits then Nothing else minimumBy (comparing (fmap (\(x,_,_)->x))) theHits

sampleSingleDiffuse :: (RandomGen g) => World -> Int -> Int -> g -> (Pixel,g)
sampleSingleDiffuse world x y g = let offset        = ((fromIntegral x / fromIntegral imageWidth) £* horizontal) £+ ((fromIntegral y / fromIntegral imageHeight) £* vertical) £- origin
                                      ray           = (origin, normalise $ lower_left_corner £+ offset)
                                      (nextRays,g2) = foldr (\_ (rays,g) -> let (nr,g2) = randomUnit g in (nr:rays,g2)) ([],g) [1..20]
                                      in (colourFuncDiffuse world ray nextRays,g2)

sampleMSAADiffuse :: (RandomGen g) => World -> Int -> Int -> g -> (Pixel,g)
sampleMSAADiffuse world x y g = let noSamples = 100
                                    noDiffuses = 50
                                    getOffset :: VecType -> VecType -> Vec3
                                    getOffset ox oy = (((ox + fromIntegral x) / fromIntegral imageWidth) £* horizontal) £+ (((oy + fromIntegral y) / fromIntegral imageHeight) £* vertical) £- origin
                                    (jxs,g2) = uniformRs noSamples (-1::VecType,1::VecType) g
                                    (jys,g3) = uniformRs noSamples (-1::VecType,1::VecType) g2
                                    (nextRays,g4) = foldr (\_ (rays,g) -> let (nr,g2) = randomUnit g in (nr:rays,g2)) ([],g3) [1..noDiffuses]
                                    jitters = zip jxs jys
                                    sampleRays = map (\(ox,oy) -> (origin, normalise $ lower_left_corner £+ (getOffset ox oy))) jitters
                                    samples = map (((£$) fromIntegral) . (\x -> colourFuncDiffuse world x nextRays)) sampleRays
                                    in (((£$) floor) . vmean $ samples,g4)


-- hard-coded image stuff
imageWidth = 400 :: Int
imageHeight = 300 :: Int

myWorld = [
    pack (( (0,0,-1), 0.5 )::Sphere),
    pack (((-0.933,-1.25,0),(0.799,-0.25,0),(-0.0670,-0.75,-1))::Plane)
    ] :: World


-- hard-coded camera stuff
viewportHeight    = 2.0                                                  :: VecType
aspectRatio       = (fromIntegral imageWidth / fromIntegral imageHeight) :: VecType
viewportWidth     = viewportHeight * aspectRatio                         :: VecType
focalLength       = 1.0                                                  :: VecType
origin            = (0,0,0)                                              :: Vec3
horizontal        = (viewportWidth, 0, 0)                                :: Vec3
vertical          = (0, viewportHeight, 0)                               :: Vec3
lower_left_corner = origin £- (0.5 £* horizontal) £- (0.5 £* vertical) £- (0,0,focalLength)


testImageDiffuseLine :: Int -> Int -> Int -> Int -> [Pixel]
testImageDiffuseLine width height line seed = fst $ foldr (\(x,y) (pxs,g) -> let (px,g2) = sampleMSAADiffuse myWorld x y g in (px:pxs,g2)) ([],mkStdGen seed) [(x,line) | x <- reverse [0..imageWidth-1]]

main = do
    putStrLn $ printf "P3\n%d\t%d\t255" imageWidth imageHeight
    time <- getUnixTime
    let seed = fromEnum . utMicroSeconds $ time
    let lineseeds = zip (reverse [0..imageHeight-1]) . fst $ uniformRs imageHeight (minBound::Int,maxBound::Int) (mkStdGen seed)
    let f = (\(y,seed) -> testImageDiffuseLine imageWidth imageHeight y seed) :: (Int,Int) -> [Pixel]
    let lines = concat $ map f lineseeds
    
    sequence_ (map (putStrLn . writePixel) lines)