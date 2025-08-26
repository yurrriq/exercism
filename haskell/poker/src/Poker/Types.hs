{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Poker.Types where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Finitary (Finitary (..))
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import Refined (FromTo, Refined, refine, refineFail, unrefine)
import Text.Trifecta (Parser, Result (..), char, natural, optional, parseString, space)

data Suit
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving stock (Eq, Generic)
  deriving anyclass (Finitary)

instance Ord Suit where
  compare _ _ = EQ

instance Show Suit where
  show = \case
    Clubs -> "C"
    Diamonds -> "D"
    Hearts -> "H"
    Spades -> "S"

data Face
  = Jack
  | Queen
  | King
  | Ace
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Finitary)

instance Show Face where
  show = \case
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

data Rank
  = Pips (Refined (FromTo 2 10) Nat)
  | Face Face
  deriving stock (Eq, Ord, Generic)

instance Show Rank where
  show (Pips n) = show (unrefine n)
  show (Face f) = show f

instance Finitary Rank where
  type Cardinality Rank = 13
  fromFinite x
    | x < 9 = either undefined Pips $ refine (2 + fromIntegral (fromEnum x))
    | otherwise = Face (fromFinite (toEnum (fromEnum x - 9)))
  toFinite (Pips n) = fromIntegral (unrefine n - 2)
  toFinite (Face f) = 9 + fromIntegral (toFinite f)

data Card
  = Card
  { cardRank :: Rank,
    cardSuit :: Suit
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Finitary)

instance Read Card where
  readsPrec _ =
    parseString card mempty >>> \case
      Success karta -> [(karta, "")]
      Failure _ -> []

instance Show Card where
  show :: Card -> String
  show (Card v s) = show v <> show s

card :: Parser Card
card = Card <$> (pipsRank <|> faceRank) <*> suit <* optional space

pipsRank :: Parser Rank
pipsRank = fmap Pips . refineFail . fromIntegral =<< natural

faceRank :: Parser Rank
faceRank =
  fmap Face $
    (Jack <$ char 'J')
      <|> (Queen <$ char 'Q')
      <|> (King <$ char 'K')
      <|> (Ace <$ char 'A')

suit :: Parser Suit
suit =
  (Clubs <$ char 'C')
    <|> (Diamonds <$ char 'D')
    <|> (Hearts <$ char 'H')
    <|> (Spades <$ char 'S')

data Hand
  = HighCard Int
  | OnePair Rank Int
  | TwoPairs Rank Rank Rank
  | ThreeOfAKind Rank Rank Rank
  | Straight Rank
  | Flush Int
  | FullHouse Rank Rank
  | FourOfAKind Rank Rank
  | StraightFlush Rank
  | RoyalFlush
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Finitary)
