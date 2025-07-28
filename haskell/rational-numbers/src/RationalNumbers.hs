module RationalNumbers
  ( Rational,
    abs,
    numerator,
    denominator,
    add,
    sub,
    mul,
    div,
    pow,
    expRational,
    expReal,
    rational,
  )
where

import Prelude hiding (Rational, abs, div)

-- Data definition -------------------------------------------------------------

data Rational a = !a :% !a deriving (Eq, Show)

rational :: (Integral a) => (a, a) -> Rational a
rational (n, d) =
  if d' < 0
    then
      negate n' :% negate d'
    else
      n' :% d'
  where
    n' = n `quot` k
    d' = d `quot` k
    k = gcd n d

-- unary operators -------------------------------------------------------------

abs :: (Integral a) => Rational a -> Rational a
abs (n :% d) =
  case (n >= 0, d >= 0) of
    (True, True) -> n :% d
    (True, False) -> n :% negate d
    (False, True) -> negate n :% d
    (False, False) -> negate n :% negate d

numerator :: (Integral a) => Rational a -> a
numerator (n :% _) = n

denominator :: (Integral a) => Rational a -> a
denominator (_ :% d) = d

-- binary operators ------------------------------------------------------------

add :: (Integral a) => Rational a -> Rational a -> Rational a
add (n1 :% d1) (n2 :% d2) = rational (n1 * d2 + n2 * d1, d1 * d2)

sub :: (Integral a) => Rational a -> Rational a -> Rational a
sub (n1 :% d1) (n2 :% d2) = rational (n1 * d2 - n2 * d1, d1 * d2)

mul :: (Integral a) => Rational a -> Rational a -> Rational a
mul (n1 :% d1) (n2 :% d2) = rational (n1 * n2, d1 * d2)

div :: (Integral a) => Rational a -> Rational a -> Rational a
div (n1 :% d1) (n2 :% d2) = rational (n1 * d2, n2 * d1)

pow :: (Integral a) => Rational a -> a -> Rational a
pow (n :% d) e
  | e >= 0 = rational (n ^ e, d ^ e)
  | otherwise = rational (d ^ negate e, n ^ negate e)

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (n :% d) e = (fromIntegral n ** e) / (fromIntegral d ** e)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (n :% d) = x ** (fromIntegral n / fromIntegral d)
