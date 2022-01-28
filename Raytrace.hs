-- The flow of the program is roughly:
-- main -> drawLine -> sample -> rayColour

import Prelude hiding ((/))
import qualified Prelude as P

import Data.List (minimumBy)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Ord (comparing)
import System.IO
import System.Random (RandomGen, mkStdGen, uniformR)
import Text.Printf (printf)

import Hittable hiding (pack)
import qualified Hittable as H
import Material hiding (pack)
import qualified Material as M
import Random
import Raylude
import Vector3

type World = [(Hittable,Material)]

-- shortcut things
(/) :: (Integral a, Fractional b) => a -> a -> b
a / b = fromIntegral a P./ fromIntegral b


-- hard-coded image/camera things
tLimits = (0.001,10000) :: (VecType,VecType)

imageWidth  = 1280 :: Int
imageHeight = 720 :: Int
aspectRatio = imageWidth / imageHeight :: VecType

viewportHeight    = 2.0 :: VecType
viewportWidth     = (viewportHeight * aspectRatio) :: VecType
focalLength       = 1.0 :: VecType
origin            = (0,0,0) :: Point
horizontal        = (viewportWidth, 0, 0) :: Vector
vertical          = (0, viewportHeight, 0) :: Vector
lower_left_corner = origin £- (0.5 £* horizontal) £- (0.5 £* vertical) £- (0,0,focalLength)

myMat = M.pack ( (0.9,0.8,0.5)::Diffuse )
myMat2 = M.pack ( (0.5,0.8,0.5)::Diffuse )
mySpecular = M.pack ( 0.2::Specular )
mySpecular2 = M.pack ( 0::Specular )
myWorld = [
    (H.pack ( ((-1.5,-0.3,-3),0.7)::Sphere ), mySpecular2),
    (H.pack ( ((0,-0.3,-4),0.7)::Sphere ), myMat),
    (H.pack ( ((1.3,-0.3,-2),0.7)::Sphere ), mySpecular),
    --(H.pack ( ((0,-100.5,-1),100)::Sphere ), myMat2)
    (H.pack ( ((-1,-1,0),(1,-1,0),(0,-1,-1))::Plane ), myMat2)
    ]


-- image writing functions
-- |Get the header for the PPM file.
imageHeader = (printf "P3\n%d\t%d\t255\n" imageWidth imageHeight) :: String

-- |Get a PPM pixel.
writePixel :: Pixel -> String
writePixel (r,g,b) = printf "%d\t%d\t%d\n" r g b


-- colour functions
-- |Get the colour of the background, that is, if the ray hits no objects.
bgColour :: Ray -> Colour
bgColour (o,d) =
    let
        t            = 0.5 * (1 + vec3y d)
        --colourTop    = pixelToColour (0xd7, 0x02, 0x70)
        colourTop = (0.5, 0.7, 1.0) 
        --colourBottom = pixelToColour (0x00, 0x38, 0xa8)
        colourBottom = (1.0,1.0,1.0)
    in
        gradient colourBottom colourTop t

-- |Convert a normalised vector to a colour
normal2rgb :: Vector -> Colour
normal2rgb = (£$) ((*0.5) . (+1))

-- |Gamma correct a colour
gammaCorrect :: Colour -> Colour
gammaCorrect = (£$) sqrt

rayColour :: (RandomGen g) => g -> World -> Ray -> (Pixel,g)
rayColour g world (o,d) =
    let
        noSamples = 100
        maxDepth = 50
        accFunc :: (RandomGen g) => a -> ([Colour],g) -> ([Colour],g)
        accFunc _ (acc,g) = let (jitter,g2) = getJitterVector g
                                (c,g3) = rayColour' g2 world (o,d £+ jitter) maxDepth
                                in (c:acc,g3)
        (samples,g4) = foldr accFunc ([],g) [1..noSamples]
    in
        (colourToPixel . gammaCorrect . vmean $ samples,g4)

-- |Calculate the colour of a particular ray
rayColour' :: (RandomGen g) => g -> World -> Ray -> Int -> (Colour,g)
rayColour' g world ray depth
    | isNothing closestHit = (bgColour ray,g)
    | depth == 0 = ((0,0,0),g)
    | otherwise = let (hr,mat) = fromJust closestHit
                      (scatterResult,g2) = scatter mat g ray hr
                      nextColourResult = (\(c,s) -> let (c2,g2) = rayColour' g world s (depth-1) in (c £** c2,g2)) <$> scatterResult
                      in fromMaybe ((0,0,0),g2) nextColourResult
    where hits = (filter $ not . isNothing) . (map (\(obj,mat) -> mergeMaybe2ple (hit tLimits ray obj,Just mat))) $ world
          closestHit = if null hits then Nothing else minimumBy (comparing $ fmap $ \((x,_,_),_) -> x) hits


-- drawing functions
-- |Get a ray that passes through pixel (x,y) of the image
getRay :: Int -> Int -> Ray
getRay x y = (origin, normalise $ lower_left_corner £+ ((x/imageWidth) £* horizontal) £+ ((y/imageHeight) £* vertical) £- origin)

getJitterVector :: (RandomGen g) => g -> (Vector,g)
getJitterVector g=
    let
        (jx,g2) = uniformR (-1/(2*imageWidth)::VecType,1/(2*imageWidth)::VecType) g
        (jy,g3) = uniformR (-1/(2*imageHeight)::VecType,1/(2*imageHeight)::VecType) g2
    in
        ((jx*viewportWidth,jy*viewportHeight,0),g3)

-- |Draw a single line of the image (0,y) -> (imageWidth-1,y)
drawLine :: (RandomGen g) => g -> Int -> [Pixel]
drawLine g y = map (fst . (rayColour g myWorld)) [getRay x y | x <- [0..imageWidth-1]]


myHeckingMonaderino :: (RandomGen g) =>  g -> Handle -> Int -> IO ()
myHeckingMonaderino rng fileHandle y = ((hPutStr fileHandle) . concat . (map writePixel) . (drawLine rng) $ y) >> (putStrLn $ printf "%d lines remaining" y)

main = do
    let rng = mkStdGen 420
    fileHandle <- openFile "image.ppm" WriteMode
    hPutStr fileHandle imageHeader
    let img = (map $ myHeckingMonaderino rng fileHandle) . reverse $ [0..imageHeight-1]

    sequence_ img
    hClose fileHandle

    --writeFile "C:\\Users\\Liz\\Documents\\image.ppm" $ imageHeader ++ (concat img)

--main = writeFile  $ imageHeader ++ (concat $ map (writePixel . bgColour) rays)
