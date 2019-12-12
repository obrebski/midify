{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Sound.MIDI.Midify.Player where

import Sound.PortMidi               (PMError,time)
import Codec.Midi                   (Message)
import Control.Monad                (when,forever)
import Control.Monad.Trans.RWS      (RWST, runRWST, get, put, modify, ask)
import Control.Concurrent           -- (ThreadId, MVar, readMVar, Chan, readChan, forkIO, threadDelay)
import Control.Monad.IO.Class       (liftIO)
import Sound.MIDI.Midify.PMWritable -- (Timestamp)

type Event      = (Double,Message)
type RealTime   = Timestamp
type TrackTime  = Double
type MidiWriter = (Timestamp, Message) -> IO PMError

data Ctrl = Ctrl { tempo :: Double
                 , delta :: Timestamp
                 } deriving Show

data PEnv = PEnv { input   :: Chan Event  -- ^ the channel MIDI events are read from
                 , output  :: MidiWriter  -- ^ a function writing timestamped MIDI events to MIDI device
                 , control :: MVar Ctrl   -- ^ shared control data
                 }
           
type Player = RWST PEnv [String] (RealTime,TrackTime) IO

runPlayer :: PEnv -> IO ThreadId
runPlayer = forkIO . runPlayer'
  where
    runPlayer' e = do now <- time
                      (a,s,t) <- runRWST (forever play) e (now,0)
                      return ()

ctrl :: (Ctrl -> a) -> Player a
ctrl f = control <$> ask >>= liftIO . readMVar >>= return . f

playTime :: Double -> Player Timestamp
playTime t = do (rt,tt) <- get
                -- if (t==0)
                --   then reset >> 
                to <- ctrl tempo
                return $ rt + round (to*(t-tt))

spendTime :: Timestamp -> Player ()
spendTime t = do d <- ctrl delta
                 when (t>d) $ liftIO $ threadDelay $ 1000 * fromIntegral (t-d `div` 2)

reset :: Player ()
reset = (,0) <$> liftIO time >>= put
           
play :: Player ()
play = do PEnv inp out _ <- ask 
          (t,m) <- liftIO $ readChan inp
          pt    <- if (t==0) then liftIO time else playTime t
          left  <- (pt -) <$> liftIO time
          spendTime left
          put (pt,t)
--          liftIO $ putStrLn $ show (pt,t)
          liftIO $ out (pt,m) >> return ()

test :: IO (ThreadId, MVar Ctrl, Chan Event)
test = do c <- newMVar (Ctrl 1000 100)
          k <- newChan :: IO (Chan Event)
          s <- start 2
          p <- runPlayer (PEnv k (writeMsg s) c)
          return (p,c,k)
          