{-# LANGUAGE DeriveFunctor #-}

module ComplexNumbers
  ( Complex,
    conjugate,
    abs,
    exp,
    real,
    imaginary,
    mul,
    add,
    sub,
    div,
    complex,
  )
where

import Data.Functor ((<&>))
import Prelude hiding (abs, div, exp)
import qualified Prelude as P

------------------------------------------------------------ [ Data definition ]

infix 6 :+

data Complex a = !a :+ !a
  deriving
    ( Eq,
      Show,
      Functor
    )

complex :: (a, a) -> Complex a
complex = uncurry (:+)

------------------------------------------------------------ [ Unary operators ]

conjugate :: Num a => Complex a -> Complex a
conjugate (a :+ b) = a :+ negate b

abs :: Floating a => Complex a -> a
abs (a :+ b) = sqrt (sqr a + sqr b)
  where
    sqr x = x * x

real :: Num a => Complex a -> a
real (a :+ _) = a

imaginary :: Num a => Complex a -> a
imaginary (_ :+ b) = b

exp :: Floating a => Complex a -> Complex a
exp (a :+ b) = expA * cos b :+ expA * sin b
  where
    expA = P.exp a

----------------------------------------------------------- [ Binary operators ]

mul :: Num a => Complex a -> Complex a -> Complex a
mul (a :+ b) (c :+ d) = (a * c - b * d) :+ (b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (a :+ b) (c :+ d) = (a + c) :+ (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub x y = add x (negate <$> y)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (a :+ b) (c :+ d) = (a * c + b * d) :+ (b * c - a * d) <&> (/ x)
  where
    x = c * c + d * d
