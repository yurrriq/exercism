{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module TwoBucket (measure) where

import Algorithm.Search (bfs)
import Control.Monad (guard)
import Data.Bifoldable (biany)
import Data.Bifunctor (bimap)
import Data.Finitary (Finitary, fromFinite, next, previous)
import Data.Finite (Finite, getFinite)
import Data.Function (fix)
import Data.List (unsnoc)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Tuple (swap)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), someNatVal, type (+))

newtype Bucket (capacity :: Nat)
  = Bucket {unBucket :: Finite (capacity + 1)}
  deriving (Eq, Generic, Ord, Enum)
  deriving anyclass (Finitary)
  deriving (Real, Integral, Num) via (Finite (capacity + 1))

instance Show (Bucket capacity) where
  show (Bucket n) = show (getFinite n)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (first, second) target
  | target > first && target > second = Nothing
  | target == first = Just (1, (first, 0))
  | target == second = Just (2, (first, fromIntegral second))
  | target `mod` gcd first second /= 0 = Nothing
  | otherwise =
      case (someNatVal (fromIntegral first), someNatVal (fromIntegral second)) of
        (SomeNat (_ :: Proxy n), SomeNat (_ :: Proxy m)) ->
          bfs
            (options @n @m)
            (biany ((target ==) . fromIntegral) ((target ==) . fromIntegral))
            (fromIntegral first, 0)
            >>= \actions ->
              (1 + length actions,) . bimap fromIntegral fromIntegral . snd
                <$> unsnoc actions

options :: (KnownNat a, KnownNat b) => (Bucket a, Bucket b) -> [(Bucket a, Bucket b)]
options (first, second) =
  catMaybes
    [ pour (first, second),
      swap <$> pour (second, first),
      (0, second) <$ guard (first > 0 && second /= full),
      (first, 0) <$ guard (first > 0 && second > 0),
      (full, second) <$ guard (second > 0),
      (first, full) <$ guard (first > 0)
    ]

pour :: forall a b. (KnownNat a, KnownNat b) => (Bucket a, Bucket b) -> Maybe (Bucket a, Bucket b)
pour (0, _) = Nothing
pour (from, to) = fix go (from, to) <$ guard (from > 0)
  where
    go _ (0, b) = (0, b)
    go rec (a, b) =
      maybe (a, b) rec $
        (,) <$> previous a <*> next b

full :: (KnownNat n) => Bucket n
full = fromFinite maxBound
