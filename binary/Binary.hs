{-# LANGUAGE LambdaCase #-}
{-|
Module      : Binary
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Converting string binary numbers to their decimal equivalents.
-}

module Binary where

import           Control.Monad (foldM)
import           Data.Maybe    (fromMaybe)

-- | Given a 'String' representing a binary number,
toDecimal :: String -> Int
toDecimal = fromMaybe 0 . foldM ((. charToBinaryM) . (<$+>) . (2*)) 0

-- | Given a 'Char', converts @'0'@ to @Just 0@ and @'1'@ to @Just 1@,
-- otherwise returns @Nothing@.
charToBinaryM :: Char -> Maybe Int
charToBinaryM = \case '0' -> Just 0; '1' -> Just 1; _ -> Nothing

-- | Given an 'Int' @x@ and a 'Maybe' @Int y@, if @y@ is a @Just@,
-- returns @Just (x + y)@, otherwise @Nothing@.
(<$+>) :: Int -> Maybe Int -> Maybe Int
(<$+>) = (<$>) . (+)
