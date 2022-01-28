module Vector3 (
    Vec3, Ray3,
    (£+), (£-), (£*), (£**), (£.), (££), (£$), (@@),
    normsquare, veclen, normalise,
    vec3x, vec3y, vec3z,
    gradient, vmean,
    isZero
)
where

-- |3-vector (x,y,z)
type Vec3 a = (a,a,a)

-- |3-ray
type Ray3 a = (Vec3 a, Vec3 a)

-- vector stuff
-- |Vector addition
(£+) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
infixl 6 £+
(ux, uy, uz) £+ (vx, vy, vz) = (ux + vx, uy + vy, uz + vz)

-- |Vector subtraction (negates addition)
(£-) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
infixl 6 £-
(ux, uy, uz) £- (vx, vy, vz) = (ux - vx, uy - vy, uz - vz)

-- |Scalar multiplication
(£*) :: (Num a) => a -> Vec3 a -> Vec3 a
infixl 7 £*
t £* (vx, vy, vz) = (t * vx, t * vy, t * vz)

-- |Component-wise multiplication of two vectors
(£**) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
infixl 7 £**
(ux, uy, uz) £** (vx, vy, vz) = (ux * vx, uy * vy, uz * vz)

-- |Dot product
(£.) :: (Num a) => Vec3 a -> Vec3 a -> a
(ux, uy, uz) £. (vx, vy, vz) = ux * vx + uy * vy + uz * vz

-- |Cross product
(££) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(ux, uy, uz) ££ (vx, vy, vz) = (uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx)

-- |Component-wise function application
(£$) :: (a -> b) -> Vec3 a -> Vec3 b
infixr 1 £$
f £$ (vx, vy, vz) = (f vx, f vy, f vz)

-- |Square of the norm of a vector
normsquare :: (Num a) => Vec3 a -> a
normsquare v = v £. v

-- |Norm/length/magnitude of a vector
veclen :: (Floating a) => Vec3 a -> a
veclen = sqrt . normsquare

-- |Normalise a vector (same direction, with magnitude 1)
normalise :: (Floating a) => Vec3 a -> Vec3 a
normalise v = (1.0 / veclen v) £* v

-- |Get x-component of a vector
vec3x :: Vec3 a -> a
vec3x (x,_,_) = x

-- |Get y-component of a vector
vec3y :: Vec3 a -> a
vec3y (_,y,_) = y

-- |Get z-component of a vector
vec3z :: Vec3 a -> a
vec3z (_,_,z) = z


-- ray stuff
-- |Find the point on the ray that is t units from the origin (o+td)
(@@) :: (Num a) => Ray3 a -> a -> Vec3 a
(o, d) @@ t = o £+ t £* d


-- vector function stuff
-- |Linear gradient between two vectors
gradient :: (Num a) => Vec3 a -> Vec3 a -> a -> Vec3 a
gradient u v t = (1-t) £* u £+ t £* v

-- |Mean of a list of vectors
vmean :: (Fractional a) => [Vec3 a] -> Vec3 a
vmean vs = (1.0 / fromIntegral (length vs)) £* (foldl1 (£+) vs)

-- swag stuff
-- |Find whether a vector is exactly zero or not
isZero :: (Num a, Eq a) => Vec3 a -> Bool
isZero = (==) (fromIntegral 0, fromIntegral 0, fromIntegral 0)