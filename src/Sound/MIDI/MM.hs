{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Sound.MIDI.MM where

import Sound.PortMidi (PMStream,time)
import Sound.MIDI.Midify (BPM,Timestamp)
import Codec.Midi
import Control.Lens
import Control.Monad      (when,forever)
import Control.Monad.Trans.RWS      ( RWST, execRWST, runRWST, get, put, modify, tell, ask )
import Control.Concurrent   ( MVar, modifyMVar_, readMVar
                            , Chan, readChan
                            , threadDelay
                            ) -- ThreadId, newMVar, newEmptyMVar, takeMVar, putMVar, withMVar, swapMVar, forkIO, forkOS)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Euterpea hiding (play,input,output,tempo,forever)
import Sound.MIDI.Midify.PMWritable


data Env = Env { _ch::Channel     -- MIDI channel
               , _bpm::BPM        -- beats per minute
               , _vel::Velocity   -- default press velocity
               , _vel'::Velocity  -- default release velocity
               } deriving Show

makeLenses ''Env

defaultEnv :: Env
defaultEnv =  Env 0 60 64 64


type MidiTrack  = Track Double

type MM = RWST ((),MVar Env) MidiTrack Double IO

see :: Getting a Env a -> MM a
see f = snd <$> ask >>= liftIO . readMVar >>= return . (^.f)

data Update f a = f := a | f :~ (a->a)

env :: Num a => [Update (ASetter Env Env a a) a] -> MM ()
env us = snd <$> ask >>= \e -> mapM_ (liftIO . modifyMVar_ e . lensify) us
  where
    lensify (f := v) = return . set f v
    lensify (f :~ g) = return . over f g

pause :: Rational -> MM ()
pause t = see bpm >>= \b -> modify (+ fromRational (t*60/b))

class Midifiable a where
  send :: a -> MM ()

instance Midifiable a => Midifiable [a] where
  send = mapM_ send

instance Midifiable Message where
  send x = get >>= \t -> tell [(t,x)]


-- instance Midifiable (Music Pitch) where
--   send (Prim (Note dur pitch))   = see vel >>= send . NoteOn' (absPitch pitch) >>
--                                    pause dur >>
--                                    see vel' >>= send . NoteOff' (absPitch pitch)
--   send (Prim (Rest dur))         = pause dur
--   send (m1 :+: m2)               = send m1 >> send m2
--   send (m1 :=: m2)               = get >>= \t -> send m1 >> get >>= \t'  ->
--                                    put t      >> send m2 >> get >>= \t'' ->
--                                    put (max t' t'')
--   send (Modify (Tempo x) m)      = loc $ env[bpm:~(* x)] >> send m
--   send (Modify (Transpose x) m)  = loc $ env[tra:=x] >> send m
--   send (Modify (Instrument x) m) = let p = fromEnum x in loc $ send (ProgramChange' p) >> env[prog:=p] >> send m


-- type Event = (Double,Message)
-- type RealTime = Double
-- type TrackTime = Double

-- data Ctrl = Ctrl { _tempo :: Double
--                  , _delta :: Timestamp
--                  } deriving Show
            
-- type MidiWriter = (Timestamp, Message) -> IO PMError

-- data PlayerEnv = PlayerEnv { _input  :: Chan Event  -- ^ the channel MIDI events are read from
--                            , _output :: MidiWriter  -- ^ a function writing timestamped MIDI events to MIDI device
--                            , _ctrl   :: MVar Ctrl   -- ^ quarter note duration in microseconds
--                            }
-- makeLenses ''Ctrl
-- makeLenses ''PlayerEnv

-- type Player = RWST PlayerEnv [String] (Timestamp,Double) IO

-- seee :: Getting a Ctrl a -> Player a
-- seee f = view ctrl <$> ask >>= liftIO . readMVar >>= return . (^.f)

-- toTimestamp :: Double -> Double -> Timestamp
-- toTimestamp to t = round $ t * 60000 / 60


-- asktempo :: Player Double
-- asktempo = view ctrl <$> ask >>= \mctrl -> view tempo <$> liftIO (readMVar mctrl)

-- askdelta :: Player Timestamp
-- askdelta = view ctrl <$> ask >>= \mctrl -> view delta <$> liftIO (readMVar mctrl)


-- playTime :: Double -> Player Timestamp
-- playTime t = do (rt,tt) <- get
--                 to <- seee tempo
--                 return $ rt + round (to * (t-tt))

-- runPlayer :: PlayerEnv -> IO ()
-- runPlayer e = do now <- time
--                  (a,s,t) <- runRWST player e (now,0)
--                  return ()



-- player :: Player ()
-- player = do t <- liftIO time
--             put (t,0)
--             forever play

-- -- nextEvent :: Player Event
-- -- nextEvent = 

-- play :: Player ()
-- play = do (t,m) <- view input <$> ask >>= liftIO . readChan
--           pt <- playTime t
--           ct <- liftIO time
--           d <- seee delta
--           liftIO $ putStrLn $ "now=" ++ show ct ++ " play time=" ++ show pt
--           when (pt - ct > d) $ liftIO $ threadDelay $ 1000 * fromIntegral (pt - ct - d `div` 2)
--           put (pt,t)
--           wt <- liftIO time
--           liftIO $ putStrLn (show (pt,m) ++ " sent at " ++ show (fromIntegral pt - fromIntegral wt) ++ "ms")
--           view output <$> ask >>= liftIO . ($ (pt,m)) 
--           return ()

