{-# LANGUAGE TemplateHaskell #-}

module Sound.MIDI.MM where

import Sound.MIDI.Midify (BPM)
import Codec.Midi
import Control.Lens
import Control.Monad      ((>=>))
import Control.Monad.Trans.RWS      ( RWST, execRWST, get, put, modify, tell, ask )
import Control.Concurrent   (MVar, modifyMVar_, readMVar) -- ThreadId, newMVar, newEmptyMVar, takeMVar, putMVar, withMVar, swapMVar, forkIO, forkOS)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Euterpea

data Env = Env { _ch::Channel     -- MIDI channel
               , _bpm::BPM        -- beats per minute
               , _vel::Velocity   -- default press velocity
               , _vel'::Velocity  -- default release velocity
               } deriving Show

makeLenses ''Env

defaultEnv :: Env
defaultEnv =  Env 0 60 64 64

type MidiTrack  = Track Double
type MidiWriter = MidiTrack -> IO ()

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


instance Midifiable (Music Pitch) where
  send (Prim (Note dur pitch))   = see vel >>= send . NoteOn' (absPitch pitch) >>
                                   pause dur >>
                                   see vel' >>= send . NoteOff' (absPitch pitch)
  send (Prim (Rest dur))         = pause dur
  send (m1 :+: m2)               = send m1 >> send m2
  send (m1 :=: m2)               = get >>= \t -> send m1 >> get >>= \t'  ->
                                   put t      >> send m2 >> get >>= \t'' ->
                                   put (max t' t'')
  send (Modify (Tempo x) m)      = loc $ env[bpm:~(* x)] >> send m
  send (Modify (Transpose x) m)  = loc $ env[tra:=x] >> send m
  send (Modify (Instrument x) m) = let p = fromEnum x in loc $ send (ProgramChange' p) >> env[prog:=p] >> send m
