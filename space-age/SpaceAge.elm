module SpaceAge exposing (Planet(..), ageOn, orbitalPeriod)

-- The eight planets in our solar system.
type Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

{- Calculate the age on a planet from an Earth age in seconds.

    round (ageOn Mercury 1000000000) == 132
    round (ageOn Venus   1000000000) == 52
    round (ageOn Earth   1000000000) == 32
-}
ageOn : Planet -> Int -> Float
ageOn = flip ((/) << toFloat) << orbitalPeriod

{- Determine a planet's orbital period in Earth seconds.

    orbitalPeriod Mercury ==  7600543.81992
    orbitalPeriod Venus   == 19414149.052176
    orbitalPeriod Earth   == 31557600
-}
orbitalPeriod : Planet -> Float
orbitalPeriod planet =
  let
    earthPeriod : Float
    earthPeriod = 31557600

    factor : Float
    factor =
      case planet of
        Mercury ->   0.2408467
        Venus   ->   0.61519726
        Earth   ->   1.0
        Mars    ->   1.8808158
        Jupiter ->  11.862615
        Saturn  ->  29.447498
        Uranus  ->  84.016846
        Neptune -> 164.79132
  in
    earthPeriod * factor
