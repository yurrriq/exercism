{-|
Module      : School
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Altering and sorting grade school rosters.
-}
module School where

import Control.Monad (ap)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A school is a map from grade to list of students.
type School  = Map Grade [Student]

-- | A grade is an integer.
type Grade   = Int

-- | A student is represented by their first name, as a string.
type Student = String

-- | The empty 'School'.
empty :: School
empty = Map.fromList []

-- | Given a 'Grade' n, a 'Student x' and a 'School' s, 'maybeAppends' @[s]@
-- to the maybe existing list of students in grade n at school s.
add :: Grade -> Student -> School -> School
add = flip $ Map.alter . maybeAppend . return

-- | Given a list of students, returns a function, that given a maybe list of
-- students, returns @Just@ the original list of students or the first list
-- appended to the second.
maybeAppend :: [Student] -> Maybe [Student] -> Maybe [Student]
maybeAppend = (Just .) . ap maybe (flip (++))

-- | Given a 'Grade' n and a 'School' s, returns the list of 'Student's
-- in grade n at school s.
grade :: Grade -> School -> [Student]
grade = (sort .) . Map.findWithDefault []

-- | Given a 'School', returns a list of pairs of a 'Grade' and a list of
-- 'Student's, sorted in ascending order by grade then by student.
sorted :: School -> [(Grade, [Student])]
sorted = Map.toAscList . Map.map sort
