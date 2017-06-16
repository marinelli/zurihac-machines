{-# LANGUAGE
    OverloadedStrings
#-}


import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Machine
import           Data.Map                (Map)
import qualified Data.Map as Map
import           Data.Set                (Set)
import qualified Data.Set as Set
import           Data.Text               (Text)
import           Text.Show.Pretty


type Interface = Text
type Timestamp = Int
type Bytes     = Int
type OrderNo   = Int


type TrafficSample = Map Interface Bytes
type Usage         = Map Interface Bytes
type Order         = Map OrderNo (Set Interface)
type Report        = Map OrderNo Bytes


type Event t = (Timestamp, t)
type Env     = Maybe Order



samples :: [(Timestamp, TrafficSample)]
samples =
    [ (0,   Map.fromList [("eth0",100), ("eth125",101), ("eth899",102)])
    , (300, Map.fromList [("eth0",200), ("eth125",201), ("eth899",202)])
    , (600, Map.fromList [("eth0",300), ("eth125",301), ("eth899",302)])
    ]


usagesReference :: [(Timestamp, Usage)]
usagesReference =
  [ (0,   Map.fromList [("eth0", 100), ("eth125", 101), ("eth899", 102)])
  , (300, Map.fromList [("eth0", 300), ("eth125", 302), ("eth899", 304)])
  , (600, Map.fromList [("eth0", 600), ("eth125", 603), ("eth899", 606)])
  ]


orders :: [(Timestamp, Order)]
orders =
    [ (250, Map.singleton 1 (Set.fromList ["eth0"]))
    , (500, Map.singleton 1 (Set.fromList ["eth125", "eth899"]))
    , (900, Map.singleton 1 (Set.fromList ["eth9"]))
    ]


reportReference :: [(Timestamp, Map Int Bytes)]
reportReference =
    [ (0,   Map.empty)            -- <- sample
    , (250, Map.singleton 1 100)  -- <- order
    , (300, Map.singleton 1 300)  -- <- sample
    , (500, Map.singleton 1 605)  -- <- order
    , (600, Map.singleton 1 1209) -- <- sample
    ]


--
-- a combinator to merge orders and usages
--
-- eg.: pPrint $ mergeEvents orders usagesReference
--
mergeEvents :: [Event Order] -> [Event Usage] -> [Event (Either Order Usage)]
mergeEvents es1 es2 =
  case
    (es1       , es2      )
  of
    ([]        , []       ) -> []
    (es1       , []       ) -> map (\ e -> (fst e, Left  $ snd e)) es1
    ([]        , es2      ) -> map (\ e -> (fst e, Right $ snd e)) es2
    (e1 : tes1 , e2 : tes2) ->
      if fst e1 <= fst e2 then
        (fst e1, Left  $ snd e1) : mergeEvents tes1 es2
      else
        (fst e2, Right $ snd e2) : mergeEvents es1 tes2


--
-- a combinator to compute the report
--
-- eg.:
--   > pPrint $ computeEvents Nothing (mergeEvents orders usagesReference)
--
--   [ ( 0 , fromList [] )
--   , ( 300 , fromList [ ( 1 , 300 ) ] )
--   , ( 600 , fromList [ ( 1 , 1209 ) ] )
--   ]
--
computeEvents :: Env -> [Event (Either Order Usage)] -> [(Timestamp, Map Int Bytes)]
computeEvents _   []       = []
computeEvents env ((t, Left  o) : es) = computeEvents (Just o) es
computeEvents env ((t, Right u) : es) =
  case env of
    Nothing -> (t, Map.empty) : computeEvents env es
    Just o  -> let order = head $ Map.toList o
                   (uid, interfaces) = (fst order, Set.toAscList $ snd order)
                   val = sum $ map
                           (\ x ->
                              case Map.lookup x u of
                                Nothing -> 0
                                Just v  -> v
                           )
                           interfaces
               in
                 (t, Map.singleton uid val) : computeEvents env es



usagePlan :: (MonadIO m)
          => Usage
          -> PlanT (Is (Timestamp, TrafficSample)) (Timestamp, Usage) m t
usagePlan usage = do

  sample <- await
  let newUsage = Map.unionWith (+) usage (snd sample)
  yield (fst sample, newUsage)
  usagePlan newUsage



usageMachine
    :: Usage
    -> MachineT IO (Is (Timestamp, TrafficSample)) (Timestamp, Usage)
usageMachine = construct . usagePlan



main :: IO ()
main =
  do
    pPrint samples

    putStr "\n\n"

    let usages = source samples ~> usageMachine Map.empty
    runT usages >>= pPrint

    putStr "\n\n"

    let report = computeEvents Nothing (mergeEvents orders usagesReference)
    pPrint report

    return ()


