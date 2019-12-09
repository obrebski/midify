{-# LANGUAGE TemplateHaskell #-}

module Engine ( module Engine
              , module Control.Lens
              , module Control.Concurrent
              , module Midify
              )

where

import Sound.PortMidi       (PMStream)
import Codec.Midi           (Message, Track)
import Control.Concurrent   (MVar, ThreadId, newMVar, newEmptyMVar, readMVar, takeMVar, putMVar, withMVar, swapMVar, forkIO, forkOS)
import Control.Monad        (forever, unless)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Lens
import Midify hiding (env)

data Status = Stop
            | Run
            deriving (Show,Eq)

data Order  = Forward
            | Backward
            | Shuffle
            | Transform (Track BeatTime -> Track BeatTime)


-- TrackMessage = Stop | Kill | Rewind
-- stopTrack - po zakończeniu
-- killTrack - natychmiast
-- modify

data Player a = Player { _status :: Status
                       , _loop :: Bool
                       , _count :: Int
                       , _stream :: PMStream
                       , _track :: MVar a
                       , _lenv :: MVar Env
                       }
            -- deriving Show

makeLenses ''Player

mkPlayer :: PMWritable a => PMStream -> IO (Player a)
mkPlayer s = Player Stop False 1 s <$> newEmptyMVar <*> newMVar defaultEnv

type Engine a = ReaderT (Player a) IO ()

-- run :: PMWritable a => Player a -> IO ThreadId
run :: Player (T RealTime ()) -> IO ThreadId
run = forkIO . engine

-- engine :: PMWritable a => Player a -> IO ()
engine :: Player (T RealTime ()) -> IO ()
engine p = forever $ do
  t <- takeMVar $ p^.track
  e <- readMVar $ p^.lenv
  putStrLn $ show $ p^.status
  write (p^.stream) $ snd $ midifyIn (0,e) t


-- -- run :: MVar Player -> IO ThreadId
-- -- run p = runReader $ forever $ do
--   -- ctrl <- newMVar
-- --  unless (null (track ctrl)) $ liftM $ write (stream ctrl) (track ctrl)




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
