module Lib.Queue
  ( Queue(push, pop, new)
  ) where

class Queue q where
  new :: q a
  push :: a -> q a -> q a
  pop :: q a -> Maybe (a, q a)
