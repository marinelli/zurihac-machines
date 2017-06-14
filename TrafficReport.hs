{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}

import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.List               (sum)
import           Data.Machine
import           Data.Machine.Concurrent ((>~>))
import qualified Data.Machine.Concurrent as CM
import           Data.Machine.Runner
import           Data.Map                (Map)
import qualified Data.Map.Strict         as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import           Text.Show.Pretty

type Interface = Text
type Timestamp = Int
type Bytes = Int
type OrderNo = Int

type TrafficSample = Map Interface Bytes
type Usage = Map Interface Bytes

type Order = Map OrderNo (Set Interface)

type Report = Map OrderNo Bytes

samples :: Source (Timestamp,TrafficSample)
samples = source (map sample [0..5])
  where
  sample s = (s*300, Map.fromList [("eth0", (s*100))
                                  ,("eth125", (s*100+1))
                                  ,("eth899", (s*100+2))])

orders :: Source (Timestamp, Order)
orders = source [(600, Map.singleton 1 (Set.fromList ["eth125","eth899"]))]

usagePlan :: (MonadIO m)
          => Usage
          -> PlanT (Is (Timestamp,TrafficSample)) (Timestamp,Usage) m t
usagePlan usage = do
  sample <- await
  let newUsage = Map.unionWith (+) usage (snd sample)
  yield (fst sample, newUsage)
  usagePlan newUsage

-- usagesReference =
--   source [(0,   Map.fromList [("eth0",  0), ("eth125",  1), ("eth899",  2)])
--          ,(300, Map.fromList [("eth0",100), ("eth125",102), ("eth899",103)])
--          ,(600, Map.fromList [("eth0",300), ("eth125",303), ("eth899",306)])
--          ,(900, Map.fromList [("eth0",600), ("eth125",604), ("eth899",608)])
--          ...
--          ]

reportCombinator :: ProcessT IO a (Timestamp,Order)
                 -> ProcessT IO b (Timestamp,Usage)
                 -> WyeT IO a b Report
reportCombinator us os = do
  let latestOrders = Map.empty
      latestUsage = Map.empty
  CM.wye us os (construct (combine latestOrders latestUsage))
  where
  combine o u = do
    x <- awaits Z
    case x of
      Left (_,o')  -> yield (recalculateOrder o' u) >>  combine o' u
      Right (_,u') -> yield (recalculateOrder o  u') >> combine o  u'
  recalculateOrder o' u' = Map.map (sumUsage u') o'
  sumUsage u' ifs = sum (u' `Map.restrictKeys` ifs)

reportReference = Map.singleton 1 3018

main :: IO ()
main = do
  let usages = samples ~> (construct (usagePlan Map.empty))
      report = reportCombinator orders usages ~> final
  runT usages >>= pPrint
  runT report >>= pPrint
