{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}


module Sound.MIDI.Midify.Player where

import Sound.PortMidi               (PMSuccess, PMError, time)
import Sound.MIDI.Midify.PMWritable -- (Timestamp)
import Sound.MIDI.Midify.Types
import Codec.Midi                   -- (Message)
import Control.Monad                (when,forever,liftM)
import Control.Monad.Trans.RWS      (RWST, runRWST, get, put, modify, ask)
import Control.Concurrent           -- (ThreadId, MVar, readMVar, Chan, readChan, forkIO, threadDelay)
import Control.Monad.IO.Class       (MonadIO,liftIO)
import Control.Lens

type MidiWriter = (PCClock, Message) -> IO (Either PMError PMSuccess)

data Status = Run | Pause
  deriving Show

data Ctrl = Ctrl { _qlen     :: TrackTime  -- ^ the length of quarter note in microseconds
                 , _delta    :: PCClock    -- ^
                 , _status   :: Status
                 } deriving Show

makeLenses ''Ctrl

data PEnv = PEnv { input   :: Chan Event  -- ^ the channel MIDI events are read from
                 , output  :: MidiWriter  -- ^ a function writing timestamped MIDI events to MIDI device
                 , control :: MVar Ctrl   -- ^ shared control data
                 }

data State = State { _ctime :: PCClock
                   , _ttime :: TrackTime
                   , _closet :: Maybe Event
                   }
            
makeLenses ''State

type Player = RWST PEnv [String] State IO

runPlayer :: PEnv -> IO ThreadId
runPlayer = forkIO . runPlayer'
  where
    runPlayer' e = do now <- time
                      (a,s,t) <- runRWST (forever play) e (State now 0 Nothing)
                      return ()

now :: Player PCClock
now = liftIO time

ctrlGet :: (Ctrl -> a) -> Player a
ctrlGet f = ask <&> control >>= liftIO . readMVar >>= return . f

ctrlMod :: (Ctrl -> Ctrl) -> Player ()
ctrlMod f = ask <&> control >>= liftIO . flip modifyMVar_ (return . f)

cc :: MonadIO m => (MVar Ctrl) -> (Ctrl -> Ctrl) -> m ()
cc v f = liftIO $ modifyMVar_ v (return . f)

playTime :: TrackTime -> Player PCClock
playTime 0 = now
playTime t = do s <- get
                q <- ctrlGet $ view qlen
                return $ s^.ctime + round (q*4*(t - s^.ttime)/1000)

spendTime :: PCClock -> Player ()
spendTime t = do d <- ctrlGet _delta
                 when (t>2*d) $ liftIO $ threadDelay $ 1000 * fromIntegral (t - d)

-- reset :: Player ()
-- reset = get >>= (,0) <$> now >>= put

next :: Player Event
next =  get <&> view closet >>= \case
                                  Nothing -> ask >>= liftIO . readChan . input
                                  Just e  -> modify (closet .~ Nothing)  >> return e

stash :: Event -> Player ()
stash e = modify $ closet .~ Just e

writeMeta :: Event -> Player ()
writeMeta (t,TempoChange c) = ctrlMod (over qlen (fromIntegral . const c))
writeMeta _                 = return ()

play :: Player ()
play = do e@(t, m) <- next
          ct       <- playTime t
          if isMetaMessage m
            then do writeMeta e
                    modify $ (ctime .~ ct) . (ttime .~ t)
            else do timeLeft  <- (ct -) <$> now
                    d <- ctrlGet $ view delta
                    if timeLeft < 2 * d
                      then do writer <- ask <&> output
                              liftIO $ writer (ct,m) >> return ()
                              modify $ (ctime .~ ct) . (ttime .~ t)
                      else do stash e
                              liftIO $ threadDelay (fromIntegral d)
          return ()

test :: IO (ThreadId, MVar Ctrl, Chan Event)
test = do c <- newMVar (Ctrl 1000000 100 Run)
          k <- newChan :: IO (Chan Event)
          s <- start 2
          p <- runPlayer (PEnv k (write' s) c)
          return (p,c,k)
          
