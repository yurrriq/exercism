{-# LANGUAGE RecordWildCards #-}

module Deque
  ( Deque,
    mkDeque,
    pop,
    push,
    shift,
    unshift,
  )
where

import Data.Functor (($>))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (uncons)

data Deque a = Deque
  { front :: IORef [a],
    back :: IORef [a]
  }

mkDeque :: IO (Deque a)
mkDeque = Deque <$> newIORef [] <*> newIORef []

pop :: Deque a -> IO (Maybe a)
pop Deque {..} = readIORef front >>= maybe popBack doPop . uncons
  where
    popBack = readIORef back >>= maybe (pure Nothing) doPop . uncons . reverse
    doPop (x, xs) = writeIORef front xs $> Just x

push :: Deque a -> a -> IO ()
push Deque {..} x = modifyIORef front (x :)

unshift :: Deque a -> a -> IO ()
unshift Deque {..} x = modifyIORef back (x :)

shift :: Deque a -> IO (Maybe a)
shift Deque {..} = pop (Deque back front)
