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

import Prelude hiding (abs, div, exp)
import qualified Prelude as P

------------------------------------------------------------ [ Data definition ]

newtype Complex a = Complex {getParts :: (a, a)}
  deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex = Complex

------------------------------------------------------------ [ Unary operators ]
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex (realPart, imaginaryPart)) =
  Complex (realPart, negate imaginaryPart)

abs :: Floating a => Complex a -> a
abs (Complex (realPart, imaginaryPart)) =
  sqrt (realPart * realPart + imaginaryPart * imaginaryPart)

real :: Num a => Complex a -> a
real = fst . getParts

imaginary :: Num a => Complex a -> a
imaginary = snd . getParts

exp :: Floating a => Complex a -> Complex a
exp (Complex (a, b)) = Complex (P.exp a * cos b, P.exp a * sin b)

----------------------------------------------------------- [ Binary operators ]

mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex (a, b)) (Complex (c, d)) = Complex (a * c - b * d, b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex (a, b)) (Complex (c, d)) = Complex (a + c, b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex (a, b)) (Complex (c, d)) = Complex (a - c, b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex (a, b)) (Complex (c, d)) =
  Complex
    ( (a * c + b * d) / (c * c + d * d),
      (b * c - a * d) / (c * c + d * d)
    )
