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
import           Text.Show.Pretty

type Interface = Text
type Timestamp = Int
type Bytes = Int
type OrderNo = Int

type TrafficSample = Map Interface Bytes
type Usage = Map Interface Bytes

type Order = Map OrderNo (Set Interface)

type Report = Map OrderNo Bytes

pushOne :: [(Timestamp,a)] -> IO (Maybe ((Timestamp,a),[(Timestamp,a)]))
pushOne []     = pure Nothing
pushOne (s:ss) = pure (Just (s, ss))


samples :: SourceT IO (Timestamp,TrafficSample)
samples =
  unfoldT pushOne [(0,   Map.fromList [("eth0",100), ("eth125",101), ("eth899",102)])
                  ,(300, Map.fromList [("eth0",200), ("eth125",201), ("eth899",202)])
                  ,(600, Map.fromList [("eth0",300), ("eth125",301), ("eth899",302)])
                  ]

orders :: SourceT IO (Timestamp, Order)
orders =
  unfoldT pushOne [(200, Map.singleton 1 (Set.fromList ["eth0"]))
                  ,(500, Map.singleton 1 (Set.fromList ["eth125", "eth899"]))]

usagePlan :: (MonadIO m)
          => Usage
          -> PlanT (Is (Timestamp,TrafficSample)) (Timestamp,Usage) m t
usagePlan usage = do
  sample <- await
  let newUsage = Map.unionWith (+) usage (snd sample)
  yield (fst sample, newUsage)
  usagePlan newUsage

usageMachine :: Usage -> MachineT IO (Is (Timestamp,TrafficSample)) (Timestamp,Usage)
usageMachine u = construct (usagePlan u)

usagesReference = [(0,   Map.fromList [("eth0",100), ("eth125",101), ("eth899",102)])
                  ,(300, Map.fromList [("eth0",300), ("eth125",302), ("eth899",303)])
                  ,(600, Map.fromList [("eth0",600), ("eth125",603), ("eth899",606)])
                  ]

reportMachine :: ProcessT IO a (Timestamp,Order)
              -> ProcessT IO b (Timestamp,Usage)
              -> WyeT IO a b String
reportMachine us os = wye us os $ repeatedly $ do
  x <- awaits Z
  case x of
    Left a  -> yield (show a)
    Right a -> yield (show a)

reportReference = [(0,   Map.empty)            -- <- sample
                  ,(250, Map.singleton 1 100)  -- <- order
                  ,(300, Map.singleton 1 300)  -- <- sample
                  ,(500, Map.singleton 1 605)  -- <- order
                  ,(600, Map.singleton 1 1209) -- <- sample
                  ]


main :: IO ()
main = do
  let usages = samples ~> usageMachine Map.empty
  runT usages >>= pPrint
  runT (reportMachine orders usages) >>= pPrint
  -- calculate report



-- counters :: Map (Interface,OrderNo) Counter
-- counters = Map.fromList
