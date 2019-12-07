module Engine where

import Codec.Midi (Message,Track)
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import PMWritable

data Status = Stop
            | Run
            deriving (Show,Eq)

data Order  = Forward
            | Backward
            | Shuffle
            | Transform (Track BPMTime -> Track BPMTime)


-- data Arp = No | Up Int | Down Int | Random | 


-- TrackMessage = Stop | Kill | Rewind
-- stopTrack - po zakończeniu
-- killTrack - natychmiast
-- modify

data Ctrl = Ctrl { status :: Status
                 , dir :: Order
                 , loop :: Bool
                 , count :: Int
                 , bpm :: Integer
                 , end :: (Ctrl -> IO Bool)
                 }

defCtrl :: Ctrl
defCtrl = Ctrl  { status = Stop
                , dir    = Forward
                , loop   = False
                , count  = 1
                , bpm    = 60
                , end    = const (return False)
                }


type Engine a = ReaderT (MVar Ctrl, MVar (Track BPMTime)) IO a

-- newEngine :: IO (ThreadId, MVar Ctrl, MVar (Track BPMTime))
-- newEngine = do ctrl <- newMVar
               


-- main = do
--     c  <- newChan
--     cs <- getChanContents c     -- a lazy stream of events from eventReader
--     forkIO (producer c)          -- char producer
--     consumer cs

--   where
--     -- thread one: the event producer
--     producer c = forever $ do
--         key <- getChar
--         writeChan c key

--     -- thread two: the lazy consumer
--     consumer = mapM_ print . map shift
--         where shift c | isAlpha c = chr (ord c + 1)
--                       | otherwise = c

-- forever a = a >> forever a

-- data Track = Track { content::[Int],  }

-- newEngine :: IO (ThreadId, MVar Ctrl, MVar [Int]))
-- newEngine = do i <- newEmptyMVar
--                s <- newMVar Run
--                let ctrl = Ctrl { info = i, status = s }
--                id <- forkIO $ run ctrl
--                return (id,ctrl)



-- run' :: Ctrl -> IO ()
-- run' ctrl = evalState run ctrl

-- run :: Engine ()
--   run = forever $ do
--   ctrl <- rea
--   s <- readMVar (status ctrl)
--   i <- tryTakeMVar (bpm ctrl)
--   when (s == Run) $ case i of
--                       Just i -> putStrLn (show i)
--                       Nothing -> return ()
--   threadDelay 1000000
                                  

-- startEngine
-- pauseEngine
-- stopEngine
-- killEngine
