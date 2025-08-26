{-# LANGUAGE RecordWildCards #-}

module Poker
  ( bestHands,
  )
where

import Control.Monad (guard)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.Finitary (fromFinite, toFinite)
import Data.Finite (finite)
import Data.Functor ((<&>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl', sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Ord (Down (..))
import Data.Tuple (swap)
import Poker.Types (Card (..), Face (..), Hand (..), Rank (..), card)
import Text.Trifecta (Result (..), count, parseString)

bestHands :: [String] -> Maybe [String]
bestHands = fmap (NE.toList . snd) . foldl' go Nothing
  where
    go Nothing x = (,) <$> bestHand x <*> Just (x :| [])
    go (Just (b, x :| xs)) y =
      bestHand y <&> \a ->
        case compare a b of
          LT -> (b, x :| xs)
          EQ -> (b, y :| (x : xs))
          GT -> (a, y :| [])

bestHand :: String -> Maybe Hand
bestHand input =
  case parseString (count 5 card) mempty input of
    Success karte -> Just (scoreHand (encodeHand karte))
    Failure _ -> Nothing

scoreHand :: (Int, IntMap Int, IntMap Int) -> Hand
scoreHand (ranks, rankFrequencies, suitFrequencies) =
  case (5 `elem` suitFrequencies, mkStraight ranks, sorted) of
    (True, Just (Face Ace), _) ->
      RoyalFlush
    (True, Just rank, _) ->
      StraightFlush rank
    (False, Nothing, [(rank4, 4), (rank1, 1)]) ->
      FourOfAKind (fromInt rank4) (fromInt rank1)
    (False, Nothing, [(rank3, 3), (rank2, 2)]) ->
      FullHouse (fromInt rank3) (fromInt rank2)
    (True, Nothing, _) ->
      Flush ranks
    (False, Just rank, _) ->
      Straight rank
    (False, Nothing, [(rank3, 3), (rankA, 1), (rankB, 1)]) ->
      ThreeOfAKind (fromInt rank3) (fromInt rankA) (fromInt rankB)
    (False, Nothing, [(rankA, 2), (rankB, 2), (rank1, 1)]) ->
      TwoPairs (fromInt rankA) (fromInt rankB) (fromInt rank1)
    (False, Nothing, (rankPair, 2) : _) ->
      OnePair (fromInt rankPair) ranks
    _nothingBetter ->
      HighCard ranks
  where
    sorted = sortOn (Down . swap) (IntMap.toList rankFrequencies)

encodeHand :: [Card] -> (Int, IntMap Int, IntMap Int)
encodeHand = foldl' go (0, IntMap.empty, IntMap.empty)
  where
    go (ranks, rankFrequencies, suitFrequencies) (Card {..}) =
      let rank = fromEnum (toFinite cardRank)
          suit = fromEnum (toFinite cardSuit)
       in ( ranks .|. (1 `shiftL` rank),
            IntMap.insertWith (+) rank 1 rankFrequencies,
            IntMap.insertWith (+) suit 1 suitFrequencies
          )

fromInt :: Int -> Rank
fromInt = fromFinite . finite . toInteger

mkStraight :: Int -> Maybe Rank
mkStraight ranks = fromInt <$> go 8
  where
    go (-1) = 3 <$ guard (isAceLowStraight ranks)
    go n
      | isMatch (0b11111 `shiftL` n) ranks = Just (n + 4)
      | otherwise = go (n - 1)

isAceLowStraight :: Int -> Bool
isAceLowStraight = isMatch 0b1000000001111

isMatch :: Int -> Int -> Bool
isMatch mask n = n .&. mask == mask
