{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Data.Map                 (Map)
import qualified Data.Map.Strict          as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import           Streaming                (Of(..), Stream(..))
import qualified Streaming                as S
import           Streaming.Internal
import qualified Streaming.Prelude        as S

type Interface = Text
type Timestamp = Int
type Bytes = Int
type OrderNo = Int

type TrafficSample = Map Interface Bytes
type Usage = Map Interface Bytes

type Order = Map OrderNo (Set Interface)

type Report = Map OrderNo Bytes

samples :: Stream (Of (Timestamp,TrafficSample)) IO ()
samples = S.take 6 $ S.each $ map sample $ [0..]
  where
  sample s = (s*300, Map.fromList [("eth0", (s*100))
                                  ,("eth125", (s*100+1))
                                  ,("eth899", (s*100+2))])

orders :: Stream (Of (Timestamp, Order)) IO ()
orders = S.each [
   (100, Map.singleton 1 (Set.fromList ["eth0"]))
  ,(600, Map.singleton 1 (Set.fromList ["eth125","eth899"]))
  ]

usages :: Stream (Of (Timestamp, Usage)) IO ()
usages = S.scan (\acc s -> (fst s, Map.unionWith (+) (snd acc) (snd s))) (0,Map.empty) id samples

report :: (Timestamp,Usage) -> (Timestamp,Order) -> Report
report (_,u) (_,o) = Map.map (sumUsage u) o
  where
  sumUsage u ifs = sum (u `Map.restrictKeys` ifs)

y :: (Show a, Show b) => Stream (Of a) IO () -> Stream (Of b) IO () -> (a -> b -> c) -> a -> b -> Stream (Of c) IO ()
y as bs f ia ib = do
  nextA <- S.lift newEmptyTMVarIO
  nextB <- S.lift newEmptyTMVarIO
  S.lift $ forkIO (streamToTMVar as nextA)
  S.lift $ forkIO (streamToTMVar bs nextB)
  combine nextA nextB f ia ib False False
  where
  streamToTMVar :: Stream (Of x) IO () -> TMVar (Maybe x) -> IO ()
  streamToTMVar xs v = do
    nx <- S.next xs
    case nx of
      Right (x, xs') -> atomically (putTMVar v (Just x)) >> streamToTMVar xs' v
      Left r         -> atomically (putTMVar v Nothing) >> return r
  combine vna vnb f a b emptyA emptyB = if (emptyA && emptyB) then Return () else do
    nv <- S.lift $ atomically ((Left <$> takeTMVar vna) `orElse` (Right <$> takeTMVar vnb))
    case nv of
      (Left !(Just v))  -> Step (f v b :> combine vna vnb f v b False emptyB)
      (Right !(Just v)) -> Step (f a v :> combine vna vnb f a v emptyA False)
      (Left Nothing)    -> combine vna vnb f a b True emptyB
      (Right Nothing)   -> combine vna vnb f a b emptyA True


yBlocking :: (Show a, Show b) => Stream (Of a) IO () -> Stream (Of b) IO () -> (a -> b -> c) -> a -> b -> Stream (Of c) IO ()
yBlocking as bs f ia ib = do
  ar <- S.lift (S.next as)
  br <- S.lift (S.next bs)
  case (ar,br) of
    (Right (nextA, restAs), Right (nextB, restBs)) -> Step ((f nextA nextB) :> y restAs restBs f nextA nextB)
    (Right (nextA, restAs), Left _) -> Step ((f nextA ib) :> y restAs bs f nextA ib)
    (Left _, Right (nextB, restBs)) -> Step ((f ia nextB) :> y as restBs f ia nextB)
    otherwise -> Return ()


-- usagesReference =
--   [(0,   Map.fromList [("eth0",  0), ("eth125",  1), ("eth899",  2)])
--   ,(300, Map.fromList [("eth0",100), ("eth125",102), ("eth899",103)])
--   ,(600, Map.fromList [("eth0",300), ("eth125",303), ("eth899",306)])
--   ,(900, Map.fromList [("eth0",600), ("eth125",604), ("eth899",608)])
--   ...
--   ]

-- reportReference = Map.singleton 1 3018

main :: IO ()
main = S.print (y (S.delay 0.1 usages) (S.delay 0.5 orders) report (0, Map.empty) (0, Map.empty))
