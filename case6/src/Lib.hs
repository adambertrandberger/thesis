module Lib
    ( 
    ) where

import Control.Concurrent

-- Practice:
-- Writer monad  
data Writer a = Writer (a, String) deriving (Show) -- we could make String a parameter (any monoid would do)

instance Functor Writer where
  fmap f (Writer (x, s)) = Writer (f x, s) 

instance Applicative Writer  where
  pure x = Writer (x, [])
  (Writer (f, s1)) <*> (Writer (y, s2)) = Writer ((f y), s1 ++ s2)

instance Monad Writer where
  return = pure
  (Writer (x, s1)) >>= f = let (Writer (y, s2)) = (f x)
                           in  (Writer (y, s1 ++ s2))

write :: String -> Writer ()
write s = Writer ((), s)

writeLn = write . (\s -> s ++ "\n")

writerAdd :: Int -> Int -> Writer Int
writerAdd x y = do
  writeLn $ "Added " ++ (show x) ++ " and " ++ (show y)
  return (x + y)

testWriter :: Writer Int
testWriter = do
  x <- writerAdd 2 4
  x <- writerAdd x 8
  return x

-- Reader monad
data Reader a = Reader (a)

-- Maybe monad
data MyMaybe a = MyJust a | MyNothing deriving (Show);

instance Functor MyMaybe where
  fmap f MyNothing = MyNothing
  fmap f (MyJust v) = MyJust $ f v

instance Applicative MyMaybe where
  pure x = (MyJust x)
  
  (MyJust f) <*> m = fmap f m
  MyNothing <*> m = MyNothing
  
instance Monad MyMaybe where
  return = pure
  m >>= f = case m of
              MyNothing -> MyNothing
              MyJust x -> f x

-- functor style
addToMaybeF :: (MyMaybe Int) -> Int -> (MyMaybe Int)  
addToMaybeF mi i = fmap (+ i) mi

-- monadic style
addToMaybeM :: (MyMaybe Int) -> Int -> (MyMaybe Int)
addToMaybeM mi i = mi >>= \x -> return (x + i)

-- monadic style with do syntax
addToMaybeMDo :: (MyMaybe Int) -> Int -> (MyMaybe Int)
addToMaybeMDo mi i = do
  v <- mi
  return (v + i)

-- applicative style
addToMaybeA :: (MyMaybe Int) -> Int -> (MyMaybe Int)
addToMaybeA mi i = (pure (+)) <*> mi <*> (pure i)


-- State monad
-- state monad is the combination of reader + writer monad
data State s a = State { runState :: s -> (a, s) }

-- Functor only works for * -> *. Not * -> * -> *.
-- so we need to fix one of the type parameters. in this case we fix "s"
-- so mapping on a State should map whatever a is
-- fmap :: (a -> b) ->  (s -> (a, s)) -> (s -> (b, s))
instance Functor (State s) where
  fmap f (State { runState=runState }) =
    State { runState=(\s ->
                        let (val, state) = runState s
                        in (f val, state))
          }

-- instance Applicative (State s) where
--   pure x = State (\s -> (x, s))
--   sm <*> v = 

-- FRP monad

type Time = Double
newtype Behavior a = Behavior { at :: Time -> a }

instance Functor Behavior where
  fmap f b = Behavior $ \t -> f $ b `at` t

instance Applicative Behavior where
  pure = return
  b1 <*> b2 = Behavior $ \t ->
    let
      f = b1 `at` t
    in
      f $ b2 `at` t

instance Monad Behavior where
  return x = Behavior $ \t -> x
  (>>=) b1 f = Behavior $ \t ->
        let 
            x = b1 `at` t
            b2 = f x
        in 
            b2 `at` t

cosB = Behavior $ \t -> cos(t)
sinB = Behavior $ \t -> sin(t)

type Coordinate = (Double, Double)

spin :: Behavior Coordinate
spin = do
  x <- cosB
  y <- sinB
  return (x, y)

setTimeout :: IO () -> Int -> IO ThreadId
setTimeout ioOperation ms =
  forkIO $ do
    threadDelay (ms*1000)
    ioOperation


data Program s a = Program {
                  step :: (s, Behavior a, Time) -> s,
                  rate :: Double
                }



-- data processing monad
-- need something to describe KPI calculations
-- things like windows as data is coming in (for computing THD etc...)
-- sometimes things hvae to be buffered until enough data comes in

data Stream a = Stream a
data DataProcess a b = DataProcess
  {
    runProcess :: (Stream a) -> (Stream b)
  }


