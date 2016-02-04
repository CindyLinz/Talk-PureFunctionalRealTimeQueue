module Main where

import Lib

import System.CPUTime
import System.Environment
import Control.DeepSeq

data Scenario a
  = Push a (Scenario a)
  | Pop (Scenario a)
  | Done
  deriving Show
instance NFData a => NFData (Scenario a) where
  rnf (Push a next) = rnf a `seq` rnf next
  rnf (Pop next) = rnf next
  rnf Done = ()

run :: (Queue q, Show a) => String -> Scenario a -> q a -> IO ()
run name scenario queue = do
  putStrLn $ "start test " ++ name

  let
    go queue !n !sumTime !maxTime !sumSqrTime = \case
      Done -> return (n, sumTime, maxTime, sumSqrTime)
      Push a next -> do
        timeBefore <- getCPUTime
        let !queue' = push a queue
        timeAfter <- getCPUTime
        let timeDiff = timeAfter - timeBefore
        --putStrLn $ "  push " ++ show a
        go queue' (n+1) (sumTime + timeDiff) (maxTime `max` timeDiff) (sumSqrTime + timeDiff*timeDiff) next
      Pop next -> do
        timeBefore <- getCPUTime
        let !(Just (a, queue')) = pop queue
        timeAfter <- getCPUTime
        let timeDiff = timeAfter - timeBefore
        --putStrLn $ "  pop " ++ show a
        go queue' (n+1) (sumTime + timeDiff) (maxTime `max` timeDiff) (sumSqrTime + timeDiff*timeDiff) next

  (n, sumTime, maxTime, sumSqrTime) <- go queue 0 0 0 0 scenario

  putStrLn $ "done test " ++ name
  putStrLn $ "  step: " ++ show n
  putStrLn $ "  total: " ++ show sumTime
  putStrLn $ "  max: " ++ show maxTime
  let avgTime = fromIntegral sumTime / fromIntegral n
  putStrLn $ "  avg: " ++ show avgTime
  putStrLn $ "  var: " ++ show (fromIntegral sumSqrTime / fromIntegral n - avgTime * avgTime)
  --putStrLn $ "  total: " ++ show (doneTime `diffUTCTime` startTime)

run2 :: Queue q => String -> Int -> q Int -> IO ()
run2 name len q = do
  putStrLn $ "start test " ++ name
  timeBefore <- getCPUTime
  let
    myQueue = foldr push q [1..len]
    bads = map (\x -> pop (push x myQueue)) [1..len]
    !headSum = foldr (\(Just (h, _)) acc -> acc + h) 0 bads
  timeAfter <- getCPUTime
  putStrLn $ "done test " ++ name ++ "\n  " ++ show (timeAfter - timeBefore)

main :: IO ()
main = do
  [sizeStr] <- getArgs

  let
    size = read sizeStr :: Int
--    scenario = force $ scHead . scMiddle . scTail $ Done
--
--    scHead = go size where
--      go 0 = id
--      go n = Push n . go (n - 1)
--
--    scTail = go size where
--      go 0 = id
--      go n = Pop . go (n - 1)
--
--    scMiddle = go size where
--      go 0 = id
--      go n = Push n . Pop . go (n - 1)
--
--  run "amortized" scenario (new :: QueueAmortized Int)
--  run "realtime" scenario (new :: QueueRealtime Int)
  run2 "amortized" size (new :: QueueAmortized Int)
  run2 "realtime" size (new :: QueueRealtime Int)
