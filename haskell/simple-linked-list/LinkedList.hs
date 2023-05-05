module LinkedList where

data LinkedList a = Nil | Node {datum :: a, next :: LinkedList a}

fromList :: [a] -> LinkedList a
fromList = foldr Node nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Node

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = flip reverse' nil
  where
    reverse' :: LinkedList a -> LinkedList a -> LinkedList a
    reverse' Nil = id
    reverse' (Node x xs) = reverse' xs . new x

toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
