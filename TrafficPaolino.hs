{-# language TemplateHaskell, OverloadedStrings, OverloadedLists, GeneralizedNewtypeDeriving, ViewPatterns#-}
import  Control.Concurrent      (forkIO, threadDelay)
import  Control.Concurrent.STM  (atomically, readTChan, writeTChan, newTChanIO)
import  Control.Monad           (forever, forM_)
import  Control.Monad.IO.Class  (liftIO)
import  Data.Map                (Map, unionWith, (!))
import  Data.Set                (Set)
import  Data.Text               (Text)
import  Data.String             (IsString)
import  Control.Arrow           (second, (&&&), (***))
import  Control.Lens            ((%~))
import  Control.Lens.TH         (makeLenses)
import  Data.Copointed          (copoint)
import  Data.Foldable           (sum, toList)
import  Data.Machine    (
                        Moore(..), unfoldMealy, unfoldMoore, 
                        yield, runT, (<~), auto, autoM, repeatedly,
                        MachineT, Mealy
                        )

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
newtype Traffic         = Traffic (Map Interface Bytes)
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
    {   _traffic        :: Moore Traffic Usage --  ^ accumulated traffic
    ,   _order          :: Order --  ^ actual order
    }

makeLenses ''State

-- | type of input for the computation
type Event = Either Traffic Order

-- | step the state given an event
step :: Event -> State -> State
step (Left x) = traffic %~ stepMoore x 
step (Right s) = order %~ (s `mappend`)

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
samples =  map (second Traffic)
   [    (0,   [("eth0",100), ("eth125",101), ("eth899",102)])
   ,    (300,  [("eth0",200), ("eth125",201), ("eth899",202)])
   ,    (600,  [("eth0",300), ("eth125",301), ("eth899",302)])
   ]

orders :: [Timed Order]
orders = map (second Order)
    [   (200, [(1, ["eth0"])])
    ,   (500, [(1 ,["eth125", "eth899"])])
    ]

-- | use timestamps to simulate arrival of events with time relative to start of program * 2000 µs
fakeSourcing :: [Timed Traffic] -> [Timed Order] -> IO (MachineT IO k (Timed (Either Traffic Order)))
fakeSourcing t o = do
    c <- newTChanIO
    forM_ (map (fmap Left) t ++ map (fmap Right) o) $ \(Timestamp t,x) -> 
        forkIO $ threadDelay (t * 2000 ) >> atomically (writeTChan c $ (Timestamp t,x))
    return $ repeatedly $ liftIO (atomically $ readTChan c) >>= yield

main = do
    src <- fakeSourcing samples orders
    -- event time just get through
    runT $ autoM print <~ (auto $ second machine) <~ src
