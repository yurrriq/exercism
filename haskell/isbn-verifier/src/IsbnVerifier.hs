module IsbnVerifier
  ( isbn,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (views)
import Control.Lens.TH (makeLenses)
import Data.Char (digitToInt)
import Data.Functor (($>))
import Text.Trifecta (Parser, Result (..), char, count, digit, eof, optional, parseString)

newtype Country
  = Country Int
  deriving (Show)

newtype Publisher
  = Publisher [Int]
  deriving (Show)

newtype Title
  = Title [Int]
  deriving (Show)

newtype CheckDigit = CheckDigit
  { unCheckDigit :: Int
  }
  deriving (Show)

data ISBN = ISBN
  { _country :: Country,
    _publisher :: Publisher,
    _title :: Title,
    _checkDigit :: CheckDigit
  }
  deriving (Show)

makeLenses ''ISBN

isbn :: String -> Bool
isbn input =
  case parseString parser mempty input of
    Success parsed ->
      sum
        [ views country checksumCountry parsed,
          views publisher checksumPublisher parsed,
          views title checksumTitle parsed,
          views checkDigit unCheckDigit parsed
        ]
        `mod` 11 == 0
    Failure _ -> False

checksumCountry :: Country -> Int
checksumCountry (Country n) = n * 10

checksumPublisher :: Publisher -> Int
checksumPublisher (Publisher digits) =
  sum $ zipWith (*) digits [9, 8, 7]

checksumTitle :: Title -> Int
checksumTitle (Title digits) =
  sum $ zipWith (*) digits [6, 5, 4, 3, 2]

parser :: Parser ISBN
parser =
  ISBN
    <$> (Country . digitToInt <$> digit) <* maybeDash
    <*> (Publisher . map digitToInt <$> count 3 digit) <* maybeDash
    <*> (Title . map digitToInt <$> count 5 digit) <* maybeDash
    <*> (CheckDigit <$> ((digitToInt <$> digit) <|> (char 'X' $> 10))) <* eof

maybeDash :: Parser (Maybe Char)
maybeDash = optional (char '-')
