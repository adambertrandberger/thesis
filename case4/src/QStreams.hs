module QStreams
( 
  )
where

import Control.Concurrent (forkIO, threadDelay, readChan, ThreadId, writeChan, newChan)
import Control.Monad (forever)

-- what is the unit node?
-- a process that takes what it is given and spits it back out

-- what does it mean to combine nodes?
-- unit * 

-- what is a unit network?
-- a network that has a source of x, and a sink of x

-- data Network a = Network [Process a a]
-- instance Functor Channel where
--   fmap f (Channel a) = (Channel (f a))

-- data Process a b = Process { outChan :: Channel b
--                            , inChan :: Channel a
--                            , f :: IO (Channel a, Channel b) -> IO()
--                          }

-- instance Functor (Process a) where
--   fmap f1 (Process { outChan=output, inChan=input, f=func }) =
--        Process {
--     outChan = output
--     , inChan = input
--                }




-- instance Functor (Process a) where

-- addOne x = x + 1

data Channel a =
  Channel a (IO (Channel a)) |
  EmptyChannel 

fromList :: [a] -> Channel a
fromList [] = (EmptyChannel)
fromList (x:xs) = (Channel x (return (fromList xs)))

setTimeout :: IO () -> Int -> IO ThreadId
setTimeout ioOperation ms =
  forkIO $ do
    threadDelay (ms*1000)
    ioOperation

makeStream :: Channel Integer
makeStream = Channel 0 (setTimeout (Channel 10 EmptyStream) 1000)

makeChannelLoop :: IO ()
makeChannelLoop = forever 

delayValue :: Integer -> a -> IO a
delayValue i x = forkIO $

