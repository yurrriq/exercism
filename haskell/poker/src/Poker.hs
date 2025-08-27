{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Poker
  ( bestHands,
  )
where

import Control.Monad (guard)
import Control.Monad.ST (runST)
import Data.Bits (Bits, shiftL, (.&.), (.|.))
import Data.Finitary (fromFinite, inhabitants, toFinite)
import Data.Finite (finite)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.List (foldl', sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Ord (Down (..))
import Data.Vector.Mutable.Sized qualified as VMS
import Data.Vector.Sized qualified as VS
import Data.Word (Word16)
import Poker.Types (Card (..), Face (..), Hand (..), HandAnalysis (..), Rank (..), card)
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

scoreHand :: HandAnalysis -> Hand
scoreHand (HandAnalysis {..}) =
  case (5 `elem` suitFrequencies, mkStraight ranks, sorted) of
    (True, Just (Face Ace), _) ->
      RoyalFlush
    (True, Just rank, _) ->
      StraightFlush rank
    (False, Nothing, (4, rank4) : (1, rank1) : _) ->
      FourOfAKind rank4 rank1
    (False, Nothing, (3, rank3) : (2, rank2) : _) ->
      FullHouse rank3 rank2
    (True, Nothing, _) ->
      Flush ranks
    (False, Just rank, _) ->
      Straight rank
    (False, Nothing, (3, rank3) : (1, rankA) : (1, rankB) : _) ->
      ThreeOfAKind rank3 rankA rankB
    (False, Nothing, (2, rankA) : (2, rankB) : (1, rank1) : _) ->
      TwoPairs rankA rankB rank1
    (False, Nothing, (2, rankPair) : _) ->
      OnePair rankPair ranks
    _nothingBetter ->
      HighCard ranks
  where
    sorted =
      sortOn Down $
        filter ((> 0) . fst) $
          zip (VS.toList rankFrequencies) inhabitants

encodeHand :: [Card] -> HandAnalysis
encodeHand cards = runST do
  rankCounts <- VMS.replicate 0
  suitCounts <- VMS.replicate 0
  let go ranks Card {..} = do
        let rank = toFinite cardRank
        VMS.modify rankCounts (+ 1) rank
        VMS.modify suitCounts (+ 1) (toFinite cardSuit)
        pure (ranks .|. (1 `shiftL` fromEnum rank))
  HandAnalysis
    <$> foldlM go 0 cards
    <*> VS.freeze rankCounts
    <*> VS.freeze suitCounts

fromInt :: Int -> Rank
fromInt = fromFinite . finite . toInteger

mkStraight :: Word16 -> Maybe Rank
mkStraight ranks = fromInt <$> go 8
  where
    go (-1) = 3 <$ guard (isAceLowStraight ranks)
    go n
      | isMatch (0b11111 `shiftL` n) ranks = Just (n + 4)
      | otherwise = go (n - 1)

isAceLowStraight :: Word16 -> Bool
isAceLowStraight = isMatch 0b1000000001111

isMatch :: (Bits a) => a -> a -> Bool
isMatch mask n = n .&. mask == mask
