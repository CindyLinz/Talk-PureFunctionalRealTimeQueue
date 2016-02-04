module Lib.QueueAmortized
  ( QueueAmortized
  ) where

import Lib.Queue

data QueueAmortized a = QueueAmortized
  [a] -- enqueue buffer
  [a] -- dequeue buffer
  deriving Show

instance Queue QueueAmortized where
  new :: QueueAmortized a
  new = QueueAmortized [] []

  push :: a -> QueueAmortized a -> QueueAmortized a
  push a (QueueAmortized fs bs) = QueueAmortized (a:fs) bs

  pop :: QueueAmortized a -> Maybe (a, QueueAmortized a)
  pop (QueueAmortized fs (b:bs)) = Just (b, QueueAmortized fs bs)
  pop (QueueAmortized [] []) = Nothing
  pop (QueueAmortized fs []) = Just (b, QueueAmortized [] bs) where
    b : bs = reverse fs
