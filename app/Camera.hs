module Camera (
    Camera,
    getRay,
    theCamera
)
where

import Prelude hiding ((/))
import qualified Prelude as P

import Raylude
import Vector3 ( (£+), (£-), (£*) )

(/) :: (Integral a, Fractional b) => a -> a -> b
a / b = fromIntegral a P./ fromIntegral b

data Camera = Camera {
    origin      :: Point,
    focalLength :: VecType,
    vfov        :: VecType,
    aspectRatio :: VecType
}

getRay :: Camera -> Int -> Int -> Int -> Int -> Ray
getRay cam w h x y =
    let 
        viewportHeight = (*2) . tan $ vfov cam
        viewportWidth = viewportHeight * aspectRatio cam
        horizontal = (viewportWidth, 0, 0)
        vertical = (0, viewportHeight, 0)
        lower_left_corner = (origin cam) £- (0.5 £* horizontal) £- (0.5 £* vertical) £- (0,0,focalLength cam)
        dx = (x/w) £* horizontal
        dy = (y/h) £* vertical
    in
        (origin cam, lower_left_corner £+ dx £+ dy £- (origin cam))

theCamera = Camera (0,0,0) 1 (pi P./ 2) (16/9)