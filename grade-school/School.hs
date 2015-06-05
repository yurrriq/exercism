{-|
Module      : School
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Altering and sorting grade school rosters.
-}
module School (
  School,
  Grade,
  Student,
  empty,
  add,
  grade,
  sorted
  ) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A school is a map from grade to list of students.
data School  = School { toMap :: Map Grade [Student]}

-- | A grade is an integer.
type Grade   = Int

-- | A student is represented by their first name, as a string.
type Student = String

-- | The empty 'School'.
empty :: School
empty = School $ Map.fromList []

-- | Given a 'Grade' @n@, a 'Student' @x@ and a 'School' @s@, appends @[x]@
-- to the existing list of 'Student's in @n@ at @s@, or sets the list to @[x]@
-- if none exists.
add :: Grade -> Student -> School -> School
add n x = School . Map.insertWith (++) n [x] . toMap 

-- | Given a 'Grade' @n@ and a 'School' @s@, returns the list of 'Student's
-- in @n@ at @s@.
grade :: Grade -> School -> [Student]
grade = sort .: Map.findWithDefault [] .:. toMap

-- | Given a 'School', returns a list of pairs of a 'Grade' and a list of
-- 'Student's, sorted in ascending order by 'Grade' then by 'Student'.
sorted :: School -> [(Grade, [Student])]
sorted = Map.toAscList . Map.map sort . toMap

-- | From "Data.Function.Pointless"
--
-- > (f .: g) x y = f (g x y)
--
-- or,
--
-- > f .: g = curry (f . uncurry g)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- |
--
-- > (f .:. g) x y = f x (g y)
--
-- or,
--
-- > f .:. g = (. g) . f
(.:.) :: (b -> c -> a) -> (d -> c) -> b -> d -> a
(.:.) = flip . ((.) .)
