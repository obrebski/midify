{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Sound.MIDI.Midify.Player where

import Sound.PortMidi               (PMSuccess, PMError, time)
import Sound.MIDI.Midify.PMWritable -- (Timestamp)
import Sound.MIDI.Midify.Types
import Codec.Midi                   -- (Message)
import Control.Monad                (when,forever)
import Control.Monad.Trans.RWS      (RWST, runRWST, get, put, modify, ask)
import Control.Concurrent           -- (ThreadId, MVar, readMVar, Chan, readChan, forkIO, threadDelay)
import Control.Monad.IO.Class       (liftIO)
import Control.Lens

type MidiWriter = (PCClock, Message) -> IO (Either PMError PMSuccess)

data Ctrl = Ctrl { _qnLength :: PCClock -- ^ the length of quarter note in microseconds
                 , _delta    :: PCClock    -- ^
                 } deriving Show

makeLenses ''Ctrl

data PEnv = PEnv { input   :: Chan Event  -- ^ the channel MIDI events are read from
                 , output  :: MidiWriter  -- ^ a function writing timestamped MIDI events to MIDI device
                 , control :: MVar Ctrl   -- ^ shared control data
                 }
           
type Player = RWST PEnv [String] (PCClock,TrackTime) IO

runPlayer :: PEnv -> IO ThreadId
runPlayer = forkIO . runPlayer'
  where
    runPlayer' e = do now <- time
                      (a,s,t) <- runRWST (forever play) e (now,0)
                      return ()

now :: Player PCClock
now = liftIO time

ctrlGet :: (Ctrl -> a) -> Player a
ctrlGet f = control <$> ask >>= liftIO . readMVar >>= return . f

ctrlMod :: (Ctrl -> Ctrl) -> Player ()
ctrlMod f = control <$> ask >>= liftIO . flip modifyMVar_ (return . f)

playTime :: TrackTime -> Player PCClock
playTime 0 = now
playTime t = do (clockTime,trackTime) <- get
                qnLength <- ctrlGet _qnLength
                return $ clockTime + round (fromIntegral qnLength*4*(t-trackTime)/1000)

spendTime :: PCClock -> Player ()
spendTime t = do d <- ctrlGet _delta
                 when (t>2*d) $ liftIO $ threadDelay $ 1000 * fromIntegral d

reset :: Player ()
reset = (,0) <$> now >>= put

nextEvent :: Player Event
nextEvent = ask >>= liftIO . readChan . input

processMeta (t,TempoChange c) = ctrlMod (over qnLength (fromIntegral . const c))
processMeta _                 = return ()

play :: Player ()
play = do e@(t, m) <- ask >>= liftIO . readChan . input
          if isMetaMessage m
            then processMeta e
            else do clockTime <- playTime t
                    timeLeft  <- (clockTime -) <$> liftIO time
                    spendTime timeLeft
                    put (clockTime,t)
                    writer <- output <$> ask
                    liftIO $ writer (clockTime,m)
                    return ()

test :: IO (ThreadId, MVar Ctrl, Chan Event)
test = do c <- newMVar (Ctrl 1000000 100)
          k <- newChan :: IO (Chan Event)
          s <- start 2
          p <- runPlayer (PEnv k (write' s) c)
          return (p,c,k)
          
