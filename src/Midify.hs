{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Midify (module Midify, module PMWritable)
where

import PMWritable
import Euterpea hiding (Message(..),first,second)
import Codec.Midi
import GHC.Exts (sortWith)
import Control.Monad.Trans.RWS
import Control.Lens
import Data.Bits
import Data.List

type BPM      = Rational

data Env = Env {
                 _time::RealTime,  -- time in seconds
                 _ch::Channel,     -- MIDI channel
                 _bpm::BPM,        -- beats per minute
                 _vel::Velocity,   -- default press velocity
                 _vel'::Velocity,  -- default release velocity
                 _tra::Int,        -- transposition
                 _prog::Int        -- program
               } deriving Show

makeLenses ''Env

env0 = Env {_time=0, _ch=0, _bpm=60, _vel=64, _vel'=64, _tra=0, _prog=0}

toPCClock :: Track RealTime -> Track PCClock
toPCClock = map $ over _1 (round . (* 1000))

instance PMWritable (Track RealTime) where
  write s = write s . toPCClock

type T = RWS Bool (Track RealTime) Env

instance PMWritable (T ()) where
  write s = write s . midify

instance Show (T ()) where
  show _ = "()"
  
midifyIn :: Env -> T () -> (Env,Track RealTime)
midifyIn env c = execRWS c True env

midify :: T () -> Track RealTime
midify = snd . midifyIn env0

see :: Getting a Env a -> T a
see f = view f <$> get

data Update f a = f := a | f :~ (a->a)

env :: Num a => [Update (ASetter Env Env a a) a] -> T ()
env = sequence_ . map (modify . toLens)
  where
    toLens (f := v) = (set f v)
    toLens (f :~ g) = (over f g)

gettime :: T RealTime
gettime = view time <$> get

settime :: RealTime -> T ()
settime = modify . set time

loc :: T () -> T ()
loc m = get >>= \e -> m >> see time >>= \t -> put (set time t e)

fork :: T () -> T ()
fork m = get >>= \e -> m >> put e

pause :: Rational -> T ()
pause t = see bpm >>= \b -> modify $ over time (+ fromRational (t * 60 / b))

every :: Rational -> [T ()] -> T ()
every t = sequence_ . intersperse (pause t)

within :: Rational -> [T ()] -> T ()
within t xs = every t' xs where t' = (t / fromIntegral (length xs - 1))

rep :: Int -> T () -> T ()
rep n = sequence_ . replicate n

class Transmissible a where
  send :: a -> T ()

instance Transmissible a => Transmissible [a] where
  send = sequence_ . map send

instance Transmissible (Env -> Env) where
  send = modify

instance Transmissible Message where
  send x = gettime >>= \t -> see tra >>= \tr -> tell [(t,transposeIfNote tr x)]
    where
      transposeIfNote tr (NoteOn  c p v) = NoteOn  c (p+tr) v
      transposeIfNote tr (NoteOff c p v) = NoteOff c (p+tr) v
      transposeIfNote _  other           = other

instance Transmissible ChannelMessage where
  send x = see ch >>= send . assignChannel x

instance Transmissible (Music Pitch) where
  send (Prim (Note dur pitch))   = see tra >>= \tr ->
                                   see vel >>= send . NoteOn' (absPitch pitch + tr) >>
                                   pause (dur * 4) >>
                                   see vel' >>= send . NoteOff' (absPitch pitch + tr)
  send (Prim (Rest dur))         = pause (dur * 4)
  send (m1 :+: m2)               = send m1 >> send m2
  send (m1 :=: m2)               = gettime >>= \t -> send m1 >> gettime >>= \t'  ->
                                   settime t      >> send m2 >> gettime >>= \t'' ->
                                   settime (max t' t'')


  send (Modify (Tempo x) m)      = loc $ env[bpm:~(* x)] >> send m
  send (Modify (Transpose x) m)  = loc $ env[tra:=x] >> send m
  send (Modify (Instrument x) m) = let p = fromEnum x in loc $ send (ProgramChange' p) >> env[prog:=p] >> send m
  
sortTrack :: (Num a, Ord a) => Track a -> Track a
sortTrack = sortWith fst


data ChannelMessage = NoteOn' Int Int
                    | NoteOn'' Int
                    | NoteOff' Int Int
                    | NoteOff'' Int
                    | KeyPressure' Int Int
                    | ProgramChange' Int
                    | ChannelPressure' Int
                    | PitchBend' Int
                    | PitchBend2' Int
                    | BankSelect' Int
                    | Modulation' Int
                    | BreathController' Int
                    | FootController' Int
                    | PortamentoTime' Int
                    | Volume' Int
                    | Balance' Int
                    | Pan' Int
                    | Expression' Int
                    | EffectController1' Int
                    | EffectController2' Int
                    | Sustain' Bool
                    | Portamento' Bool
                    | Sostenuto' Bool
                    | SoftPedal' Bool
                    | Legato' Bool
                    | Hold2' Bool
                    | SoundVariation' Int
                    | SoundResonance' Int
                    | SoundRelease' Int
                    | SoundAttack' Int
                    | SoundCutoff' Int
                    | AllSoundsOff'
                    | ResetAllControllers'
                    | LocalControl' Bool
                    | AllNotesOff'
                    | OmniOff'
                    | OmniOn'
                    | MonoMode'
                    | MonoModeN' Int
                    | PolyMode'
                    | Int' Int
                    deriving Show

u4  = 0x0F
u7  = 0x7F
u14 = 0x3FFF

class Assignable a where
  assignChannel :: a -> (Channel -> Message)

instance Assignable ChannelMessage where
  assignChannel (NoteOn' x y)            = \ch -> NoteOn          (ch.&.u4)   (x.&.u7)    (y.&.u7)
  assignChannel (NoteOff' x y)           = \ch -> NoteOff         (ch.&.u4)   (x.&.u7)    (y.&.u7)
  assignChannel (KeyPressure' x y)       = \ch -> KeyPressure     (ch.&.u4)   (x.&.u7)    (y.&.u7)
  assignChannel (ProgramChange' x)       = \ch -> ProgramChange   (ch.&.u4)   (x.&.u7)
  assignChannel (ChannelPressure' x)     = \ch -> ChannelPressure (ch.&.u4)   (x.&.u7)
  assignChannel (PitchBend' x)           = \ch -> PitchWheel      (ch.&.u4)   (shift ((x + 0x3F).&.u7) 8)
  assignChannel (PitchBend2' x)          = \ch -> PitchWheel      (ch.&.u4)   ((x+0x1FFF).&.u14)
  
  assignChannel (BankSelect' x)          = \ch -> ControlChange (ch.&.u4)   0           (x.&.u7)
  assignChannel (Modulation' x)          = \ch -> ControlChange (ch.&.u4)   1           (x.&.u7)
  assignChannel (BreathController' x)    = \ch -> ControlChange (ch.&.u4)   2           (x.&.u7)
  assignChannel (FootController' x)      = \ch -> ControlChange (ch.&.u4)   4           (x.&.u7)
  assignChannel (PortamentoTime' x)      = \ch -> ControlChange (ch.&.u4)   5           (x.&.u7)
  assignChannel (Volume' x)              = \ch -> ControlChange (ch.&.u4)   7           (x.&.u7)
  assignChannel (Balance' x)             = \ch -> ControlChange (ch.&.u4)   8           ((x + 0x3F).&.u7)
  assignChannel (Pan' x)                 = \ch -> ControlChange (ch.&.u4)  10           ((x + 0x3F).&.u7)
  assignChannel (Expression' x)          = \ch -> ControlChange (ch.&.u4)  11           (x.&.u7)
  assignChannel (EffectController1' x)   = \ch -> ControlChange (ch.&.u4)  12           (x.&.u7)
  assignChannel (EffectController2' x)   = \ch -> ControlChange (ch.&.u4)  13           (x.&.u7)
  assignChannel (Sustain' x)             = \ch -> ControlChange (ch.&.u4)  64           (if x then 0x40 else 0x00)
  assignChannel (Portamento' x)          = \ch -> ControlChange (ch.&.u4)  65           (if x then 0x40 else 0x00)
  assignChannel (Sostenuto' x)           = \ch -> ControlChange (ch.&.u4)  66           (if x then 0x40 else 0x00)
  assignChannel (SoftPedal' x)           = \ch -> ControlChange (ch.&.u4)  67           (if x then 0x40 else 0x00)
  assignChannel (Legato' x)              = \ch -> ControlChange (ch.&.u4)  68           (if x then 0x40 else 0x00)
  assignChannel (Hold2' x)               = \ch -> ControlChange (ch.&.u4)  69           (if x then 0x40 else 0x00)
  assignChannel (SoundVariation' x)      = \ch -> ControlChange (ch.&.u4)  70           (x.&.u7)
  assignChannel (SoundResonance' x)      = \ch -> ControlChange (ch.&.u4)  71           (x.&.u7)
  assignChannel (SoundRelease' x)        = \ch -> ControlChange (ch.&.u4)  72           (x.&.u7)
  assignChannel (SoundAttack' x)         = \ch -> ControlChange (ch.&.u4)  73           (x.&.u7)
  assignChannel (SoundCutoff' x)         = \ch -> ControlChange (ch.&.u4)  74           (x.&.u7)
  assignChannel (AllSoundsOff')          = \ch -> ControlChange (ch.&.u4) 120           0x00
  assignChannel (ResetAllControllers')   = \ch -> ControlChange (ch.&.u4) 121           0x00
  assignChannel (LocalControl' x)        = \ch -> ControlChange (ch.&.u4) 122           (if x then 127 else 0)
  assignChannel (AllNotesOff')           = \ch -> ControlChange (ch.&.u4) 123           0x00
  assignChannel (OmniOff')               = \ch -> ControlChange (ch.&.u4) 124           0x00
  assignChannel (OmniOn')                = \ch -> ControlChange (ch.&.u4) 125           0x00
  assignChannel (MonoMode')              = \ch -> ControlChange (ch.&.u4) 126           0x00
  assignChannel (MonoModeN' x)           = \ch -> ControlChange (ch.&.u4) 126           (x.&.u7)
  assignChannel (PolyMode')              = \ch -> ControlChange (ch.&.u4) 127           0x00


-- NOTATKI

b x  = send (PitchBend' x)
o    = send AllNotesOff'
oo   = sequence_ [loc (env[ch:=n] >> o) | n <- [0..15]]
pc x = send (ProgramChange' x)

-- -- komunikaty Control Change
-- ccBankSelect          = flip ControlChange   0
-- ccModulation          = flip ControlChange   1 
-- ccBreathController    = flip ControlChange   2
-- ccFootController      = flip ControlChange   4
-- ccPortamentoTime      = flip ControlChange   5
-- ccVolume              = flip ControlChange   7
-- ccBalance             = flip ControlChange   8
-- ccPan                 = flip ControlChange  10
-- ccExpression          = flip ControlChange  11
-- ccEffectController1   = flip ControlChange  12
-- ccEffectController2   = flip ControlChange  13
-- ccSustain             = flip ControlChange  64
-- ccPortamento          = flip ControlChange  65
-- ccSostenuto           = flip ControlChange  66
-- ccSoftPedal           = flip ControlChange  67
-- ccSegato              = flip ControlChange  68
-- ccHold2               = flip ControlChange  69
-- ccSoundVariation      = flip ControlChange  70
-- ccSoundResonance      = flip ControlChange  71
-- ccSoundRelease        = flip ControlChange  72
-- ccSoundAttack         = flip ControlChange  73
-- ccSoundCutoff         = flip ControlChange  74
-- ccAllSoundsOff        = flip ControlChange 120
-- ccResetAllControllers = flip ControlChange 121
-- ccLocalSwitch         = flip ControlChange 122
-- ccAllNotesOff         = flip ControlChange 123
-- ccOmniOff             = flip ControlChange 124
-- ccOmniOn              = flip ControlChange 125
-- ccMonoMode            = flip ControlChange 126
-- ccPolyMode            = flip ControlChange 127



