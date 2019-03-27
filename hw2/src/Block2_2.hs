module Block2_2
  ( moving
  ) where

import Control.Monad.State (State, evalState, modify, state)

data Queue a = Queue
  { pushStack :: [a]
  , popStack  :: [a]
  } deriving (Show)

emptyQueue :: Queue Double
emptyQueue = Queue [] []

type QueueState = State (Queue Double)

transfer :: Queue Double -> Queue Double
transfer queue@(Queue _ []) = transfer' queue
  where
    transfer' :: Queue Double -> Queue Double
    transfer' q@(Queue []     _) = q
    transfer'   (Queue (x:xs) b) = transfer' (Queue xs (x : b))
transfer q                  = q

push :: Double -> QueueState ()
push x = modify $ \q -> q { pushStack = x : pushStack q }

pop :: QueueState Double
pop = state pop'
  where
    pop' :: Queue Double -> (Double, Queue Double)
    pop' (Queue _  []    ) = error "Pop from empty list"
    pop' (Queue ys (x:xs)) = (x, Queue ys xs)

moving :: Int -> [Double] -> [Double]
moving sz list = evalState (movingHelper list 0 0 []) emptyQueue
  where
    doubleSz :: Double
    doubleSz = fromIntegral sz

    movingHelper :: [Double] -> Double -> Double -> [Double] -> QueueState [Double]
    movingHelper []     _     _   res = return (reverse res)
    movingHelper (x:xs) curSz sma res
      | curSz < doubleSz = do
          let sma' = (sma * curSz + x) / (curSz + 1)
          push x
          movingHelper xs (curSz + 1) sma' (sma' : res)
      | otherwise        = do
          modify transfer
          el <- pop
          push x
          let sma' = sma + (x - el) / curSz
          movingHelper xs curSz sma' (sma' : res)
