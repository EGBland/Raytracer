module Raylude (
    VecType, Point, Vector, Ray, Colour, Pixel,
    pixelToColour, colourToPixel,
    mergeMaybe2ple, mergeMaybe3ple
)
where

import qualified Vector3 as V

-- |The type that the ray tracer's vectors use. Full functionality requires that VecType is an instance of Floating.
type VecType = Double

-- |A point in 3D space.
type Point   = V.Vec3 VecType

-- |A vector/direction in 3D space.
type Vector  = V.Vec3 VecType

-- |A ray, the heart of the ray tracer.
type Ray     = V.Ray3 VecType

-- |A colour (r,g,b), where each component varies from 0 to 1.
type Colour  = V.Vec3 VecType

-- |A pixel (r,g,b), where each component varies from 0 to 255, integers.
type Pixel   = V.Vec3 Int

-- |Convert a pixel (rgb, 0-255 int) to a colour (rgb, 0-1 floating)
pixelToColour :: Pixel -> Colour
pixelToColour = (V.£$) $ (/255) . fromIntegral

-- |Convert a colour (rgb, 0-1 floating) to a pixel (rgb, 0-255 int)
colourToPixel :: Colour -> Pixel
colourToPixel = (V.£$) $ floor . (*255)


-- utilities
-- |Merge a 2-tuple of Maybes into a Maybe of a 2-tuple.
mergeMaybe2ple :: (Maybe a, Maybe b) -> Maybe (a,b)
mergeMaybe2ple (a,b) = do
    a' <- a
    b' <- b
    return (a', b')

-- |Merge a 3-tuple of Maybes into a Maybe of a 3-tuple.
mergeMaybe3ple :: (Maybe a, Maybe b, Maybe c) -> Maybe (a,b,c)
mergeMaybe3ple (a,b,c) = do
    a' <- a
    b' <- b
    c' <- c
    return (a', b', c')