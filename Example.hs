{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Machine
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Text               (Text)

type Interface = Text
type Timestamp = Int
type Bytes = Int
type OrderNo = Int

type TrafficSample = Map Interface Bytes
type Usage = Map Interface Bytes

type Order = Map OrderNo (Set Interface)

type Report = Map OrderNo Bytes

samples :: [(Timestamp, TrafficSample)]
samples = [(0,   Map.fromList [("a",100), ("b",101), ("c",102)])
          ,(300, Map.fromList [("a",200), ("b",201), ("c",202)])
          ,(600, Map.fromList [("a",300), ("b",301), ("c",302)])
          ]

sampless :: Source (Timestamp, TrafficSample)
sampless = source samples

orderss :: Source (Timestamp, Order)
orderss = source orders

orders :: [(Timestamp,Order)]
orders = [(250, Map.singleton 1 (Set.fromList ["a"]))
         ,(500, Map.singleton 1 (Set.fromList ["b","c"]))]

result = [(0,  Map.empty)             -- <- sample
         ,(250, Map.singleton 1 100)  -- <- order
         ,(300, Map.singleton 1 300)  -- <- sample
         ,(500, Map.singleton 1 606)  -- <- order
         ,(600, Map.singleton 1 1209) -- <- sample
         ]

usagePlan :: (MonadIO m) => Usage -> PlanT (Is (Timestamp,TrafficSample)) Usage m t
usagePlan usage = do
  sample <- await
  let newUsage = Map.unionWith (+) usage (snd sample)
  yield newUsage
  usagePlan newUsage

main :: IO ()
main = do
  let usage = Map.empty
  runT (sampless ~> construct (usagePlan usage)) >>= print
  -- calculate report



-- counters :: Map (Interface,OrderNo) Counter
-- counters = Map.fromList
