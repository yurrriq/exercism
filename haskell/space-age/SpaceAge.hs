{-# LANGUAGE LambdaCase #-}

module SpaceAge where

data Planet = Earth
            | Jupiter
            | Mars
            | Mercury
            | Neptune
            | Saturn
            | Uranus
            | Venus

ageOn :: Planet -> Integer -> Float
ageOn = flip ((/) . fromInteger) . orbitalPeriod

orbitalPeriod :: Planet -> Float
orbitalPeriod = (31557600 *) . \case
  Earth   ->   1.0
  Jupiter ->  11.862615
  Mars    ->   1.8808158
  Mercury ->   0.2408467
  Neptune -> 164.79132
  Saturn  ->  29.447498
  Uranus  ->  84.016846
  Venus   ->   0.61519726
