module Vector3 (
    Vec3, Ray3,
    (£+), (£-), (£*), (£.), (££), (£$), (@@),
    norm, veclen, normalise,
    vec3x, vec3y, vec3z,
    gradient, vmean,
    isZero
)
where

type Vec3 a = (a,a,a)
type Ray3 a = (Vec3 a, Vec3 a)

-- vector stuff
(£+) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
infixl 6 £+
(ux, uy, uz) £+ (vx, vy, vz) = (ux + vx, uy + vy, uz + vz)

(£-) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
infixl 6 £-
(ux, uy, uz) £- (vx, vy, vz) = (ux - vx, uy - vy, uz - vz)

(£*) :: (Num a) => a -> Vec3 a -> Vec3 a
infixl 7 £*
t £* (vx, vy, vz) = (t * vx, t * vy, t * vz)

(£.) :: (Num a) => Vec3 a -> Vec3 a -> a
(ux, uy, uz) £. (vx, vy, vz) = ux * vx + uy * vy + uz * vz

(££) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(ux, uy, uz) ££ (vx, vy, vz) = (uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx)

(£$) :: (a -> b) -> Vec3 a -> Vec3 b
infixr 1 £$
f £$ (vx, vy, vz) = (f vx, f vy, f vz)

norm :: (Num a) => Vec3 a -> a
norm v = v £. v

veclen :: (Floating a) => Vec3 a -> a
veclen = sqrt . norm

normalise :: (Floating a) => Vec3 a -> Vec3 a
normalise v = (1.0 / veclen v) £* v

vec3x :: Vec3 a -> a
vec3x (x,_,_) = x

vec3y :: Vec3 a -> a
vec3y (_,y,_) = y

vec3z :: Vec3 a -> a
vec3z (_,_,z) = z

(@@) :: (Num a) => Ray3 a -> a -> Vec3 a
(o, d) @@ t = o £+ t £* d

-- vector function stuff
gradient :: (Num a) => Vec3 a -> Vec3 a -> a -> Vec3 a
gradient u v t = (1-t) £* u £+ t £* v

vmean :: (Fractional a) => [Vec3 a] -> Vec3 a
vmean vs = (1.0 / fromIntegral (length vs)) £* (foldl1 (£+) vs)

-- swag stuff
isZero :: (Num a, Eq a) => Vec3 a -> Bool
isZero = (==) (fromIntegral 0, fromIntegral 0, fromIntegral 0)