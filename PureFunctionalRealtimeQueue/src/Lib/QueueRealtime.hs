module Lib.QueueRealtime
  ( QueueRealtime
  ) where

import Lib.Queue

data QueueRealtime a = QueueRealtime
  Int -- enqueue buffer length
  Int -- dequeue buffer length
  [a] -- enqueue buffer
  (Moving a) -- dequeue buffer (possibly in progress)
  deriving Show

instance Queue QueueRealtime where
  new :: QueueRealtime a
  new = QueueRealtime 0 0 [] (Concating [] [])

  push :: a -> QueueRealtime a -> QueueRealtime a
  push a (QueueRealtime enN deN enBuffer deBuffer)
    | enN == deN =
      QueueRealtime 0 (deN + enN + 1) [] $! startMoving enBuffer' deBuffer
    | otherwise =
      QueueRealtime (enN + 1) deN enBuffer' $! moveMoving deBuffer
    where
      enBuffer' = a : enBuffer

  pop :: QueueRealtime a -> Maybe (a, QueueRealtime a)
  pop (QueueRealtime enN deN enBuffer deBuffer)
    | deN == 0 = Nothing
    | enN == deN = let !as' = startMoving enBuffer as in Just (a, QueueRealtime 0 (deN + enN - 1) [] as')
    | otherwise = let !as' = moveMoving as in Just (a, QueueRealtime enN (deN - 1) enBuffer as')
    where
      (a, as) = dequeueMoving deBuffer

data Moving a
  = Reversing
    [a] -- 等待翻轉
    [a] -- 已被翻轉
    [a] -- 翻完以後要接的頭部
  | Concating
    [a] -- 要被 force 脊椎的 list
    [a] -- 一步步 force 脊椎用的 handle
  -- 最終完成態為 Concating cs []
  deriving Show

-- 開始一個搬動流程
startMoving :: [a] -> Moving a -> Moving a
startMoving as (Concating bs _) = moveMoving (Reversing as [] bs)
startMoving _ other = error $ showMoving other

-- 搬動一步
moveMoving :: Moving a -> Moving a
moveMoving (Reversing (a:as) bs hs) = Reversing as (a:bs) hs
moveMoving (Reversing [] bs hs) = moveMoving (Concating cs cs) where
  cs = hs ++ bs
moveMoving (Concating cs (h:hs)) = Concating cs hs
moveMoving (Concating cs []) = Concating cs []

-- dequeue 出一項
dequeueMoving :: Moving a -> (a, Moving a)
dequeueMoving (Reversing as bs (h:hs)) = (h, Reversing as bs hs)
dequeueMoving (Reversing [] (b:bs) []) = (b, Reversing [] bs [])
dequeueMoving (Concating (c:cs) hs) = (c, Concating cs hs)
dequeueMoving other = error $ showMoving other

showMoving (Reversing as bs cs) = "Reversing " ++ show (length as) ++ " " ++ show (length bs) ++ " " ++ show (length cs)
showMoving (Concating as bs) = "Concating " ++ show (length as) ++ " " ++ show (length bs)
