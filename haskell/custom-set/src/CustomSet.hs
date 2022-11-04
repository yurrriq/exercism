{-# LANGUAGE BangPatterns #-}

module CustomSet
  ( delete,
    difference,
    empty,
    fromList,
    insert,
    intersection,
    isDisjointFrom,
    isSubsetOf,
    member,
    null,
    size,
    toList,
    union,
  )
where

import qualified Data.Foldable as Foldable
import Prelude hiding (foldl, foldr, null)

data CustomSet a
  = Node !a !(CustomSet a) !(CustomSet a)
  | Nil
  deriving (Eq, Show)

instance Ord a => Monoid (CustomSet a) where
  mempty = empty
  mconcat = unions
  mappend = (<>)

instance Ord a => Semigroup (CustomSet a) where
  (<>) = union

instance Foldable CustomSet where
  fold = go
    where
      go Nil = mempty
      go (Node x Nil Nil) = x
      go (Node x setA setB) = go setA `mappend` (x `mappend` go setB)
  {-# INLINEABLE fold #-}
  foldr = foldr
  {-# INLINE foldr #-}
  foldl = foldl
  {-# INLINE foldl #-}

delete :: Ord a => a -> CustomSet a -> CustomSet a
delete !_ Nil = Nil
delete x (Node y setA setB)
  | x < y = Node y (delete x setA) setB
  | x > y = Node y setA (delete y setB)
  | otherwise = error "NYI"
{-# INLINE delete #-}

difference :: CustomSet a -> CustomSet a -> CustomSet a
difference _setA _setB = error "You need to implement this function."

empty :: CustomSet a
empty = Nil
{-# INLINE empty #-}

fromList :: Ord a => [a] -> CustomSet a
fromList = Foldable.foldr insert Nil

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert x Nil = Node x Nil Nil
insert x set@(Node y setA setB)
  | x < y = Node y (insert x setA) setB
  | x > y = Node y setA (insert x setB)
  | otherwise = set

intersection :: CustomSet a -> CustomSet a -> CustomSet a
intersection _setA _setB = error "You need to implement this function."

isDisjointFrom :: CustomSet a -> CustomSet a -> Bool
isDisjointFrom _setA _setB = error "You need to implement this function."

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf Nil _ = True
isSubsetOf _ Nil = False
isSubsetOf (Node x Nil Nil) setB = member x setB
isSubsetOf setA setB
  | setA == setB = True
  | otherwise = error "NYI"

member :: Ord a => a -> CustomSet a -> Bool
member _ Nil = False
member x (Node y setA setB)
  | x < y = member x setA
  | x > y = member x setB
  | otherwise = True
{-# INLINE member #-}

null :: CustomSet a -> Bool
null Nil = True
null Node {} = False
{-# INLINE null #-}

size :: CustomSet a -> Int
size Nil = 0
size (Node _ setA setB) = 1 + size setA + size setB
{-# INLINE size #-}

toList :: CustomSet a -> [a]
toList = toAscList

toAscList :: CustomSet a -> [a]
toAscList = foldr (:) []

-- toDescList :: CustomSet a -> [a]
-- toDescList = foldl (flip (:)) []

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union setA Nil = setA
union setA (Node x Nil Nil) = insert x setA
union (Node x Nil Nil) setB = insert x setB
union Nil setB = setB
union _ _ = error "NYI"

unions :: (Foldable f, Ord a) => f (CustomSet a) -> CustomSet a
unions = Foldable.foldl' union empty

foldr :: (a -> b -> b) -> b -> CustomSet a -> b
foldr f = go
  where
    go z Nil = z
    go z (Node x setA setB) = go (f x (go z setB)) setA

foldl :: (a -> b -> a) -> a -> CustomSet b -> a
foldl f = go
  where
    go z Nil = z
    go z (Node x setA setB) = go (f (go z setA) x) setB
