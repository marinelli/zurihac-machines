{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
import Control.Arrow          (second, (&&&), (***))
import Control.Concurrent     (forkIO, threadDelay)
import Control.Concurrent.STM (TChan(..), atomically, newTChanIO, readTChan,
                               writeTChan)
import Control.Lens           ((%~), (.~))
import Control.Lens.TH        (makeLenses)
import Control.Monad          (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Copointed         (copoint)
import Data.Foldable          (sum, toList)
import Data.Machine           (MachineT, Mealy, Moore(..), auto, autoM, final,
                               repeatedly, runT, unfoldMealy, unfoldMoore,
                               yield, (<~))
import Data.Map               (Map, unionWith, (!))
import Data.Set               (Set)
import Data.String            (IsString)
import Data.Text              (Text)
import Text.Show.Pretty       (pPrint)

---------------- types -------------------------------------------------
-- | name of an interface
newtype Interface       = Interface Text     deriving (Eq,Ord,IsString, Show)
-- | time as an integer
newtype Timestamp       = Timestamp Int     deriving (Num,Ord,Eq, Show)
-- | number of bytes
newtype Bytes           = Bytes Int         deriving (Num,Ord,Eq, Show)
-- | ordern number
newtype OrderNo         = OrderNo Int         deriving (Num,Ord,Eq, Show)

-- | partial traffic for each interface
newtype Traffic         = Traffic (Map Interface Bytes) deriving (Show)
-- | total traffic for each interface
newtype Usage           = Usage (Map Interface Bytes) deriving (Show)
-- | order update
newtype Order           = Order (Map OrderNo (Set Interface)) deriving (Monoid)
-- | number of bytes for any order (does it make sense?)
newtype Report          = Report (Map OrderNo Bytes) deriving (Show)

-- | anything tagged with time
type Timed a = (Timestamp, a)

----------------------------- library ----------------------------
-- | step a moore machine feeding  the input
stepMoore :: a -> Moore a t -> Moore a t
stepMoore x (Moore _ f) = f x

------------ logic ---------------------------------------


-- | state of the Mealy machine.
data State = State
    {   _traffic :: Moore Traffic Usage --  ^ accumulated traffic
    ,   _order   :: Order --  ^ actual order
    }

makeLenses ''State

-- | type of input for the computation
type Event = Either Traffic Order

-- | step the state given an event
step :: Event -> State -> State
step (Left x)  = traffic %~ stepMoore x
step (Right s) = order .~ s

-- | compute a 'Report' from a 'State'
report :: State -> Report
report (State (copoint -> Usage u) (Order o)) = Report $ (sum . map (u !) . toList) <$> o


-- | a 'Moore' machine taking a 'Traffic' and exposing the accumulated 'Usage'
-- we use a Moore machine as the 'Usage' must be sampled on any 'Event'
-- on 'Traffic' event we update internal Map Interface Bytes and expose it with 'Usage'
usage :: Moore Traffic Usage
usage = flip unfoldMoore mempty $ Usage &&& flip (\(Traffic u') -> unionWith (+) u')

-- | a 'Mealy' machine closed on a 'State' which compute a new report on any 'Event'
-- un each event we step the 'State' and spit out a new 'Report'
machine :: Mealy Event Report
machine = unfoldMealy (fmap (report &&& id) . flip step) $ State usage mempty

-------------------- application ----------------------

samples :: [Timed Traffic]
-- samples =  map (second Traffic)
--    [    (0,   [("eth0",100), ("eth125",101), ("eth899",102)])
--    ,    (300,  [("eth0",200), ("eth125",201), ("eth899",202)])
--    ,    (600,  [("eth0",300), ("eth125",301), ("eth899",302)])
--    ]

samples = map (second Traffic . sample) [0..100000]
    where
    sample :: Int -> (Timestamp, Map Interface Bytes)
    sample s = (Timestamp s
             ,[("eth0", Bytes (s*100))
              ,("eth125", Bytes (s*100+1))
              ,("eth899", Bytes (s*100+2))])

orders :: [Timed Order]
orders = map (second Order)
    [   (200, [(1, ["eth0"])])
    ,   (9000, [(1 ,["eth125", "eth899"])])
    ]

fakeProducing :: TChan (Timed (Either Traffic Order)) -> [Timed Traffic] -> [Timed Order] -> IO ()
fakeProducing chan t o =
    forM_ (map (fmap Left) t ++ map (fmap Right) o) $ \(Timestamp t,x) ->
        forkIO $ threadDelay t >> atomically (writeTChan chan (Timestamp t,x))

-- | use timestamps to simulate arrival of events with time relative to start of program * 2000 µs
fakeSourcing :: TChan (Timed (Either Traffic Order)) -> MachineT IO k (Timed (Either Traffic Order))
fakeSourcing c = repeatedly (liftIO (atomically $ readTChan c) >>= yield)

main = do
    -- event time just get through
    chan <- newTChanIO
    fakeProducing chan samples orders
    runT $ autoM print <~ auto (second machine) <~ fakeSourcing chan
