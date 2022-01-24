module SimEq (
    Expression3, Equation3, System3,
    eval, solve3
)
where

type Expression3 a = (a,a,a)
type Equation3 a = (Expression3 a,a) -- ax+by+cz=d
type System3 a = (Equation3 a, Equation3 a, Equation3 a)

a :: Equation3 a -> a
a ((x,_,_),_) = x

b :: Equation3 a -> a
b ((_,y,_),_) = y

c :: Equation3 a -> a
c ((_,_,z),_) = z

d :: Equation3 a -> a
d ((_,_,_),w) = w

eval :: (Num a) => Expression3 a -> (a,a,a) -> a
eval (a,b,c) (x,y,z) = a*x + b*y + c*z

op :: (a -> a -> a) -> Equation3 a -> Equation3 a -> Equation3 a
op f ((x1,y1,z1),w1) ((x2,y2,z2),w2) = ((f x1 x2, f y1 y2, f z1 z2), f w1 w2)

(£$) :: (a -> b) -> Equation3 a -> Equation3 b
infixr 1 £$
f £$ ((x,y,z),w) = ((f x, f y, f z), f w)

(£+) :: (Num a) => Equation3 a -> Equation3 a -> Equation3 a
(£+) = op (+)

(£-) :: (Num a) => Equation3 a -> Equation3 a -> Equation3 a
(£-) = op (-)

(£*) :: (Num a) => a -> Equation3 a -> Equation3 a
(£*) = (£$) . (*)

solve3 :: (Fractional a) => System3 a -> (a,a,a)
solve3 (e1,e2,e3)
    = let e1_ref   = (fromIntegral 1 / a e1) £* e1
          e2_sube1 = e2 £- (a e2 £* e1_ref)
          e3_sube1 = e3 £- (a e3 £* e1_ref)

          e2_ref = (fromIntegral 1 / b e2_sube1) £* e2_sube1
          e3_sube1e2 = e3_sube1 £- (b e3_sube1 £* e2_ref)

          e3_rref = (fromIntegral 1 / c e3_sube1e2) £* e3_sube1e2
          e2_rref = e2_ref £- ((c e2_ref) £* e3_rref)
          e1_rref = e1_ref £- ((b e1_ref) £* e2_rref) £- ((c e1_ref £* e3_rref))
          in (d e1_rref, d e2_rref, d e3_rref)

myEq1 = ((1,1,1),5) :: Equation3 Float
myEq2 = ((3,6,4),10) :: Equation3 Float
myEq3 = ((9,4,1),8) :: Equation3 Float

main = putStrLn . show . solve3 $ (myEq1, myEq2, myEq3)