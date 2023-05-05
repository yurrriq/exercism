module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    singleton,
    insert,
    fromList,
    toList,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (Lens', (%~))
import Control.Monad (liftM2)
import Data.Bool (bool)
import Data.List (foldl')
import Data.Maybe (fromJust)

type NodeModifier a = (Maybe (BST a) -> Maybe (BST a))

data BST a = Node
  { bstValue :: a,
    bstLeft :: Maybe (BST a),
    bstRight :: Maybe (BST a)
  }
  deriving (Eq, Show)

insert :: Ord a => a -> BST a -> BST a
insert x = leftOrRight (upsert x) =<< (>= x) . bstValue

singleton :: Ord a => a -> BST a
singleton x = Node x Nothing Nothing

fromList :: Ord a => [a] -> BST a
fromList (x : xs) = foldl' (flip insert) (singleton x) xs
fromList [] = error "BST cannot be empty"

toList :: BST a -> [a]
toList (Node v l r) = fromJust $ mconcat [go l, Just [v], go r]
  where
    go = fmap toList

left :: Ord a => Lens' (BST a) (Maybe (BST a))
left f (Node v l r) = flip (Node v) r <$> f l

right :: Ord a => Lens' (BST a) (Maybe (BST a))
right f (Node v l r) = Node v l <$> f r

upsert :: Ord a => a -> NodeModifier a
upsert x = (<|> Just (singleton x)) . (insert x <$>)

leftOrRight :: (Ord a) => NodeModifier a -> Bool -> (BST a -> BST a)
leftOrRight = liftM2 bool (right %~) (left %~)
