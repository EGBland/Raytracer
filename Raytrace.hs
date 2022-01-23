{-# LANGUAGE FlexibleInstances #-}

import Control.Monad (sequence_)
import Data.List (minimum, minimumBy)
import Data.Maybe (isNothing, fromJust)
import Data.Ord (comparing)
import System.Random
import Text.Printf

import qualified Vector3
import Vector3 ( (£+), (£-), (£*), (£.), (££), (£$), (@@), norm, normalise, gradient, vec3x, vec3y, vec3z, vmean )

type VecType = Float
type Vec3 = Vector3.Vec3 VecType
type Pixel = (Int,Int,Int)
type Ray = Vector3.Ray3 VecType

type Sphere = (Vec3, VecType)
type World = [Sphere]

-- object stuff
class Hittable a where
    hits   :: Ray -> a -> Maybe VecType
    normal :: a -> Vec3 -> Vec3

instance Hittable Sphere where
    (origin,direction) `hits` (centre,radius)
        | discriminant < 0 = Nothing
        | hitTs == [] = Nothing
        | otherwise = Just (minimum hitTs)
        where a = direction £. direction
              b = 2 * (direction £. (origin £- centre))
              c = (origin £- centre) £. (origin £- centre) - (radius*radius)
              discriminant = b*b - 4*a*c
              hitTs = filter (>0) $ [(-b - sqrt discriminant) / (2*a), (-b + sqrt discriminant) / (2*a)]
    
    normal (c,_) p = normalise (p £- c)


-- utility stuff
mergeMaybe2ple :: (Maybe a, Maybe b) -> Maybe (a,b)
mergeMaybe2ple (a,b) = do
    a' <- a
    b' <- b
    return (a',b')

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
randomUnitPolar g = let (r,    g2) = uniformR (0.001::VecType,0.999::VecType) g
                        (theta,g3) = uniformR ( 0::VecType,2*pi::VecType) g2
                        (phi,  g4) = uniformR ( 0::VecType,2*pi::VecType) g3
                        sintheta   = sin theta
                        costheta   = cos theta
                        sinphi     = sin phi
                        cosphi     = cos phi -- idk if these cache
                        in ((r*sintheta*cosphi, r*sintheta*sinphi, r*costheta),g4)

randomUnitRejection :: (RandomGen g) => g -> (Vec3,g)
randomUnitRejection g = let (rs,g2) = uniformRs 3 (-1::VecType,1::VecType) g
                            vec = (rs!!0,rs!!1,rs!!2)
                            in if norm vec < 1 then (vec,g2) else randomUnit g2

randomUnit :: (RandomGen g) => g -> (Vec3,g)
randomUnit = randomUnitPolar

clamp :: (Ord a) => a -> a -> a -> a
clamp theMin theMax x
    | x < theMin = theMin
    | x > theMax = theMax
    | otherwise  = x

-- image stuff
createP3 :: Int -> Int -> [Pixel] -> String
createP3 width height pixels = (printf "P3\n%d\t%d\t255\n" width height) ++ (concat . (map writePixel) $ pixels)

writePixel :: Pixel -> String
writePixel (r,g,b) = printf "%d\t%d\t%d\n" r g b

gammaCorrect :: Vec3 -> Pixel
gammaCorrect = (£$) $ floor . (*255) . sqrt


-- hard-coded image stuff
imageWidth = 400 :: Int
imageHeight = 300 :: Int

colourFunc :: World -> Ray -> Pixel
colourFunc world (o,d)
    | isNothing closestHit = let t = vec3y d in (floor . (*255)) £$ (gradient (0.5, 0.7, 1.0) (1.0,1.0,1.0) (0.5 * (t + 1.0)))
    | otherwise = let (chObj,chT) = fromJust closestHit
                      in norm2rgb . (normal chObj) $ ((o,d) @@ chT)
    where theHits = filter (not . isNothing) . map (\x -> mergeMaybe2ple (Just x,(o,d) `hits` x)) $ world
          closestHit = if theHits == [] then Nothing else minimumBy (comparing (fmap snd)) theHits
          norm2rgb :: Vec3 -> Pixel
          norm2rgb v = floor . (*255) . (*0.5) . (+1.0) £$ v

colourFuncDiffuse :: World -> Ray -> [Vec3] -> Pixel
colourFuncDiffuse world ray nextRays = floor . (*255) £$ colourFuncDiffuse' world ray nextRays
--colourFuncDiffuse = (\(vec,g) -> ((floor . (*255)) £$ vec,g)) . colourFuncDiffuse'

colourFuncDiffuse' :: World -> Ray -> [Vec3] -> Vec3
colourFuncDiffuse' world (o,d) [] = (0,0,0)
colourFuncDiffuse' world (o,d) (nextRay:rays)
    | isNothing closestHit = let t = vec3y d in gradient (0.5, 0.7, 1.0) (1.0,1.0,1.0) (0.5 * (t + 1.0))
    | otherwise = let (chObj,chT) = fromJust closestHit
                      intersection = ((o,d) @@ chT)
                      theNormal = normal chObj intersection
                      diffuseRay = (intersection, normalise $ nextRay £+ theNormal)
                      nextColour = colourFuncDiffuse' world diffuseRay rays
                      in 0.5 £* nextColour
    where theHits = filter (not . isNothing) . map (\x -> mergeMaybe2ple (Just x,(o,d) `hits` x)) $ world
          closestHit = if theHits == [] then Nothing else minimumBy (comparing (fmap snd)) theHits

myWorld = [( (0,0,-1), 0.5 ), ( (0,-100.5, -1), 100 )]

-- hard-coded camera stuff
viewportHeight    = 2.0                                                  :: VecType
aspectRatio       = (fromIntegral imageWidth / fromIntegral imageHeight) :: VecType
viewportWidth     = viewportHeight * aspectRatio                         :: VecType
focalLength       = 1.0                                                  :: VecType
origin            = (0,0,0)                                              :: Vec3
horizontal        = (viewportWidth, 0, 0)                                :: Vec3
vertical          = (0, viewportHeight, 0)                               :: Vec3
lower_left_corner = origin £- (0.5 £* horizontal) £- (0.5 £* vertical) £- (0,0,focalLength)

rays = let fx :: Int -> VecType
           fx x = fromIntegral x / fromIntegral imageWidth
           fy :: Int -> VecType
           fy y = fromIntegral y / fromIntegral imageHeight
           gx :: Int -> Vec3
           gx x = (fx x) £* horizontal
           gy :: Int -> Vec3
           gy y = (fy y) £* vertical
           in [(origin, normalise $ lower_left_corner £+ (gx x) £+ (gy y) £- origin) | y <- reverse [0..imageHeight-1], x <- [0..imageWidth-1]]

sampleSingle :: World -> Int -> Int -> Pixel
sampleSingle world x y = let offset = ((fromIntegral x / fromIntegral imageWidth) £* horizontal) £+ ((fromIntegral y / fromIntegral imageHeight) £* vertical) £- origin
                             ray    = (origin, normalise $ lower_left_corner £+ offset)
                             in colourFunc world ray

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

-- this sucks
sampleSSAA :: World -> Int -> Int -> Pixel
sampleSSAA world x y = let getOffset :: VecType -> VecType -> Vec3
                           getOffset ox oy = (((ox + fromIntegral x) / fromIntegral imageWidth) £* horizontal) £+ (((oy + fromIntegral y) / fromIntegral imageHeight) £* vertical) £- origin
                           sampleRays = map (\dir -> (origin, normalise $ lower_left_corner £+ dir)) [getOffset (-0.5) (-0.5), getOffset 0.5 (-0.5), getOffset (-0.5) 0.5, getOffset 0.5 0.5]
                           samples = map (((£$) fromIntegral) . colourFunc world) sampleRays
                           in ((£$) floor) . vmean $ samples

sampleMSAA :: World -> Int -> Int -> [(VecType,VecType)] -> Pixel
sampleMSAA world x y rands = let getOffset :: VecType -> VecType -> Vec3
                                 getOffset ox oy = (((ox + fromIntegral x) / fromIntegral imageWidth) £* horizontal) £+ (((oy + fromIntegral y) / fromIntegral imageHeight) £* vertical) £- origin
                                 sampleRays = map (\(ox,oy) -> (origin, normalise $ lower_left_corner £+ (getOffset ox oy))) rands
                                 samples = map (((£$) fromIntegral) . colourFunc world) sampleRays
                                 in ((£$) floor) . vmean $ samples


samplePixel :: World -> Int -> Int -> Pixel
samplePixel = sampleSSAA


-- image fns
testImage1 :: Int -> Int -> [Pixel]
testImage1 width height = let fx :: Int -> Int
                              fx x = floor (255*fromIntegral x / fromIntegral width)
                              fy :: Int -> Int
                              fy y = floor (255*fromIntegral y / fromIntegral height)
                              in [(fx x, 0, fy y) | y <- [0..height-1], x <- [0..width-1]]

testImage2 :: Int -> Int -> [Pixel]
testImage2 width height = [samplePixel [] x y | y <- reverse [0..imageHeight-1], x <- [0..imageWidth-1]]

testImage3 :: Int -> Int -> [Pixel]
testImage3 width height = [samplePixel myWorld x y | y <- reverse [0..imageHeight-1], x <- [0..imageWidth-1]]

testImageMSAA :: Int -> Int -> [Pixel]
testImageMSAA width height = let f :: (RandomGen g1, RandomGen g2) => (g1,g2) -> (Int,Int) -> (Pixel,(g1,g2))
                                 f (g1,g2) (x,y) = let range     = (-1,1) :: (Float,Float)
                                                       (jxs,g12) = uniformRs 8 range g1
                                                       (jys,g22) = uniformRs 8 range g2
                                                       jitters   = zip jxs jys
                                                       in (sampleMSAA myWorld x y jitters, (g12,g22))
                                 in fst $ foldr (\pos (pxs,gs) -> let (px,gs2) = f gs pos in (px:pxs,gs2)) ([],(mkStdGen 69,mkStdGen 7779)) [(x,y) | y <- reverse [0..imageHeight-1], x <- reverse [0..imageWidth-1]]

testImageDiffuse :: Int -> Int -> [Pixel]
testImageDiffuse width height = fst $ foldr (\(x,y) (pxs,g) -> let (px,g2) = sampleSingleDiffuse myWorld x y g in (px:pxs,g2)) ([],mkStdGen 7000) [(x,y) | y <- reverse [0..imageHeight-1], x <- reverse [0..imageWidth-1]]

testImageDiffuseLine :: Int -> Int -> Int -> Int -> [Pixel]
testImageDiffuseLine width height line seed = fst $ foldr (\(x,y) (pxs,g) -> let (px,g2) = sampleMSAADiffuse myWorld x y g in (px:pxs,g2)) ([],mkStdGen seed) [(x,line) | x <- reverse [0..imageWidth-1]]

--main = putStrLn . (createP3 imageWidth imageHeight) $ (testImageDiffuseLine imageWidth imageHeight)

main = do
    putStrLn $ printf "P3\n%d\t%d\t255" imageWidth imageHeight
    let lineseeds = zip (reverse [0..imageHeight-1]) . fst $ uniformRs imageHeight (minBound::Int,maxBound::Int) (mkStdGen 420)
    let monads = map (\(y,seed) -> putStrLn . concat . (map writePixel) $ testImageDiffuseLine imageWidth imageHeight y seed) lineseeds
    sequence_ monads