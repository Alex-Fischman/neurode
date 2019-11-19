module Dual (Dual (..), evaluate, differentiate) where

data Dual a = Dual a a deriving (Read, Show, Eq, Ord)

evaluate :: Dual a -> a
evaluate (Dual u _) = u

differentiate :: Dual a -> a
differentiate (Dual _ u') = u'

instance Enum a => Enum (Dual a) where
    toEnum n            = Dual (toEnum n) (toEnum 0)
    fromEnum (Dual u _) = fromEnum u

instance Num a => Num (Dual a) where
    (Dual u u') + (Dual v v') = Dual (u + v) (u' + v')
    (Dual u u') * (Dual v v') = Dual (u * v) (u' * v + u * v')
    negate (Dual u u')        = Dual (negate u) (negate u')
    abs (Dual u u')           = Dual (abs u) (u' * signum u)
    signum (Dual u _)         = Dual (signum u) 0
    fromInteger u             = Dual (fromInteger u) 0

instance Fractional a => Fractional (Dual a) where
    (Dual u u') / (Dual v v') = Dual (u / v) ((u' * v - u * v') / (v * v))
    fromRational u            = Dual (fromRational u) 0

instance Floating a => Floating (Dual a) where
    pi                = Dual pi 0
    exp (Dual u u')   = Dual (exp u)   (u' * exp u)
    log (Dual u u')   = Dual (log u)   (u' / u)
    sin (Dual u u')   = Dual (sin u)   ( u' * cos u)
    cos (Dual u u')   = Dual (cos u)   (-u' * sin u)
    asin (Dual u u')  = Dual (asin u)  ( u' / sqrt (1 - u ** 2))
    acos (Dual u u')  = Dual (acos u)  (-u' / sqrt (1 - u ** 2))
    atan (Dual u u')  = Dual (atan u)  (u' / (1 + u ** 2))
    sinh (Dual u u')  = Dual (sinh u)  (u' * cosh u)
    cosh (Dual u u')  = Dual (cosh u)  (u' * sinh u)
    asinh (Dual u u') = Dual (asinh u) (u' / sqrt (1 + u ** 2))
    acosh (Dual u u') = Dual (acosh u) (u' / sqrt (u ** 2 - 1))
    atanh (Dual u u') = Dual (atanh u) (u' / (1 - u ** 2))
