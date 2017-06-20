{-# language TemplateHaskell, OverloadedStrings, OverloadedLists, GeneralizedNewtypeDeriving #-}
import  Control.Concurrent      (forkIO, threadDelay)
import  Control.Concurrent.STM  (atomically, readTChan, writeTChan, newTChanIO)
import  Control.Monad           (forever, forM_)
import  Control.Monad.IO.Class  (liftIO)
import  Data.Map                (Map, unionWith, (!))
import  Data.Set                (Set)
import  Data.Text               (Text)
import  Data.String             (IsString)
import  Control.Arrow           (second)
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

newtype Interface       = Interface Text     deriving (Eq,Ord,IsString, Show)
newtype Timestamp       = Timestamp Int     deriving (Num,Ord,Eq, Show)
newtype Bytes           = Bytes Int         deriving (Num,Ord,Eq, Show)
newtype OrderNo         = OrderNo Int         deriving (Num,Ord,Eq, Show)


newtype Traffic         = Traffic (Map Interface Bytes)
newtype Usage           = Usage (Map Interface Bytes) deriving (Show)
newtype Order           = Order (Map OrderNo (Set Interface)) deriving (Monoid)
newtype Report          = Report (Map OrderNo Bytes) deriving (Show)

type Timed a = (Timestamp, a)

----------------------------- library ----------------------------
-- step a moore machine
stepMoore :: a -> Moore a t -> Moore a t
stepMoore x (Moore _ f) = f x

------------ logic ---------------------------------------
-- state of the Mealy machine, we can't keep separate traffic and orders
-- due to cross query for Report
data State = State 
    {   _traffic        :: Moore Traffic Usage --  accumulate traffic
    ,   _order          :: Order --  actual order
    }

makeLenses ''State

-- step the state given an event
step :: Either Traffic Order -> State -> State
step (Left x) = traffic %~ stepMoore x 
step (Right s) = order %~ (s `mappend`)

-- compute the report 
mkReport :: Usage -> Order -> Report
mkReport (Usage b) (Order s) = Report $ (sum . map (b !) . toList) <$> s

-- traffic accumulator machine definition
keepTraffic :: Moore Traffic Usage
keepTraffic = flip unfoldMoore mempty $ \u -> (Usage u,\(Traffic u') -> unionWith (+) u u')

-- main mealy machine (filter) 
report :: Mealy (Either Traffic Order) Report
report = flip unfoldMealy (State keepTraffic mempty) $ \s e -> 
    let s'@(State t o) = step e s
    in (mkReport (copoint t) o, s')

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

-- use timestamps to fire events with time relative to start of program * 2000 µs
fakeSourcing :: [Timed Traffic] -> [Timed Order] -> IO (MachineT IO k (Timed (Either Traffic Order)))
fakeSourcing t o = do
    c <- newTChanIO
    forM_ (map (fmap Left) t ++ map (fmap Right) o) $ \(Timestamp t,x) -> 
        forkIO $ threadDelay (t * 2000 ) >> atomically (writeTChan c $ (Timestamp t,x))
    return $ repeatedly $ liftIO (atomically $ readTChan c) >>= yield

main = do
    src <- fakeSourcing samples orders
    runT $ autoM print <~ (auto $ second report) <~ src
