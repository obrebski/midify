{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Sound.MIDI.Midify where

import Codec.Midi
import Control.Lens
-- import Sound.MIDI.Midify.PMWritable ( PMWritable, Timestamp, write )
-- import GHC.Exts                     ( sortWith )
import Control.Monad.Trans.RWS      ( RWST, execRWST, get, put, modify, tell, ask )
import Data.List                    ( intersperse )
import Sound.MIDI.Midify.Types

data Env = Env { _ch   :: Channel     -- ^ MIDI channel
               , _vel  :: Velocity    -- ^ default press velocity
               , _vel' :: Velocity    -- ^ default release velocity
               } deriving Show

makeLenses ''Env

defaultEnv :: Env
defaultEnv = Env 0 64 64

type T = RWST Bool MidiTrack (TrackTime,Env) IO

midifyIn :: (TrackTime,Env) -> T () -> IO ((TrackTime,Env),MidiTrack)
midifyIn (t,env) c = execRWST c True (t,env)

midify :: T () -> IO MidiTrack
midify t = snd <$>  midifyIn (0,defaultEnv) t

see :: Getting a Env a -> T a
see f = view (_2.f) <$> get

data Update f a = f := a | f :~ (a->a)

env :: Num a => [Update (ASetter Env Env a a) a] -> T ()
env = sequence_ . map (modify . toLens)
  where
    toLens (f := v) = (set (_2.f) v)
    toLens (f :~ g) = (over (_2.f) g)

gettime :: T TrackTime
gettime = view _1 <$> get

settime :: TrackTime -> T ()
settime = modify . set _1

loc :: T () -> T ()
loc m = get >>= \(_,e) -> m >> gettime >>= \t -> put (t,e)

fork :: T () -> T ()
fork m = get >>= \s -> m >> put s

pause :: Rational -> T ()
pause t = modify $ over _1 (+ fromRational t)

every :: Rational -> [T ()] -> T ()
every t = sequence_ . intersperse (pause t)

within :: Rational -> [T ()] -> T ()
within t xs = every t' xs where t' = (t / fromIntegral (length xs - 1))

rep :: Int -> T () -> T ()
rep n = sequence_ . replicate n

class Midifiable a where
  send :: a -> T ()

instance Midifiable a => Midifiable [a] where
  send = mapM_ send

instance Midifiable Message where
  send x = gettime >>= \t -> tell [(t,x)]

data Message' = NoteOff'         !Key !Velocity
              | NoteOn'          !Key !Velocity
              | KeyPressure'     !Key !Pressure
              | ControlChange'   !Int !Int
              | ProgramChange'   !Preset
              | ChannelPressure' !Pressure
              | PitchWheel'      !PitchWheel
              deriving (Show,Eq)

instance Midifiable Message' where
  send (NoteOff' a b)       = see ch >>= \c -> send (NoteOff c a b)
  send (NoteOn' a b)        = see ch >>= \c -> send (NoteOn c a b)
  send (KeyPressure' a b)   = see ch >>= \c -> send (KeyPressure c a b)
  send (ControlChange' a b) = see ch >>= \c -> send (ControlChange c a b)
  send (ProgramChange' a)   = see ch >>= \c -> send (ProgramChange c a)
  send (ChannelPressure' a) = see ch >>= \c -> send (ChannelPressure c a)
  send (PitchWheel' a)      = see ch >>= \c -> send (PitchWheel c a)

data Message'' = NoteOff'' !Key
               | NoteOn''  !Key
               deriving (Show,Eq)

instance Midifiable Message'' where
  send (NoteOff'' k) = see vel' >>= \v -> send (NoteOff' k v)
  send (NoteOn''  k) = see vel  >>= \v -> send (NoteOn'  k v)

data CCMessage = BankSelect !Channel !Int
--                     | PitchBend' Int
--                     | PitchBend2' Int
--                     | Modulation' Int
--                     | BreathController' Int
--                     | FootController' Int
--                     | PortamentoTime' Int
--                     | Volume' Int
--                     | Balance' Int
--                     | Pan' Int
--                     | Expression' Int
--                     | EffectController1' Int
--                     | EffectController2' Int
--                     | Sustain' Bool
--                     | Portamento' Bool
--                     | Sostenuto' Bool
--                     | SoftPedal' Bool
--                     | Legato' Bool
--                     | Hold2' Bool
--                     | SoundVariation' Int
--                     | SoundResonance' Int
--                     | SoundRelease' Int
--                     | SoundAttack' Int
--                     | SoundCutoff' Int
--                     | AllSoundsOff'
--                     | ResetAllControllers'
--                     | LocalControl' Bool
               | AllNotesOff !Channel
--                     | OmniOff'
--                     | OmniOn'
               | MonoMode !Channel
--                     | MonoModeN' Int
--                     | PolyMode'
--                     | Int' Int
               deriving Show


instance Midifiable CCMessage where
  send (AllNotesOff c) = send (ControlChange c 123 0x00)
  send (MonoMode c)    = send (ControlChange c 126 0x00)


data CCMessage' = AllNotesOff'
                | MonoMode'

instance Midifiable CCMessage' where
  send AllNotesOff' = see ch >>= send . AllNotesOff
  send MonoMode' = see ch >>= send . MonoMode



-- u4  = 0x0F
-- u7  = 0x7F
-- u14 = 0x3FFF

-- class Assignable a where
--   assignChannel :: a -> (Channel -> Message)

-- instance Assignable ChannelMessage where
--   assignChannel (NoteOn' x y)            = \ch -> NoteOn          (ch.&.u4)   (x.&.u7)    (y.&.u7)
--   assignChannel (NoteOff' x y)           = \ch -> NoteOff         (ch.&.u4)   (x.&.u7)    (y.&.u7)
--   assignChannel (KeyPressure' x y)       = \ch -> KeyPressure     (ch.&.u4)   (x.&.u7)    (y.&.u7)
--   assignChannel (ProgramChange' x)       = \ch -> ProgramChange   (ch.&.u4)   (x.&.u7)
--   assignChannel (ChannelPressure' x)     = \ch -> ChannelPressure (ch.&.u4)   (x.&.u7)
--   assignChannel (PitchBend' x)           = \ch -> PitchWheel      (ch.&.u4)   (shift ((x + 0x3F).&.u7) 8)
--   assignChannel (PitchBend2' x)          = \ch -> PitchWheel      (ch.&.u4)   ((x+0x1FFF).&.u14)
  
--   assignChannel (BankSelect' x)          = \ch -> ControlChange (ch.&.u4)   0           (x.&.u7)
--   assignChannel (Modulation' x)          = \ch -> ControlChange (ch.&.u4)   1           (x.&.u7)
--   assignChannel (BreathController' x)    = \ch -> ControlChange (ch.&.u4)   2           (x.&.u7)
--   assignChannel (FootController' x)      = \ch -> ControlChange (ch.&.u4)   4           (x.&.u7)
--   assignChannel (PortamentoTime' x)      = \ch -> ControlChange (ch.&.u4)   5           (x.&.u7)
--   assignChannel (Volume' x)              = \ch -> ControlChange (ch.&.u4)   7           (x.&.u7)
--   assignChannel (Balance' x)             = \ch -> ControlChange (ch.&.u4)   8           ((x + 0x3F).&.u7)
--   assignChannel (Pan' x)                 = \ch -> ControlChange (ch.&.u4)  10           ((x + 0x3F).&.u7)
--   assignChannel (Expression' x)          = \ch -> ControlChange (ch.&.u4)  11           (x.&.u7)
--   assignChannel (EffectController1' x)   = \ch -> ControlChange (ch.&.u4)  12           (x.&.u7)
--   assignChannel (EffectController2' x)   = \ch -> ControlChange (ch.&.u4)  13           (x.&.u7)
--   assignChannel (Sustain' x)             = \ch -> ControlChange (ch.&.u4)  64           (if x then 0x40 else 0x00)
--   assignChannel (Portamento' x)          = \ch -> ControlChange (ch.&.u4)  65           (if x then 0x40 else 0x00)
--   assignChannel (Sostenuto' x)           = \ch -> ControlChange (ch.&.u4)  66           (if x then 0x40 else 0x00)
--   assignChannel (SoftPedal' x)           = \ch -> ControlChange (ch.&.u4)  67           (if x then 0x40 else 0x00)
--   assignChannel (Legato' x)              = \ch -> ControlChange (ch.&.u4)  68           (if x then 0x40 else 0x00)
--   assignChannel (Hold2' x)               = \ch -> ControlChange (ch.&.u4)  69           (if x then 0x40 else 0x00)
--   assignChannel (SoundVariation' x)      = \ch -> ControlChange (ch.&.u4)  70           (x.&.u7)
--   assignChannel (SoundResonance' x)      = \ch -> ControlChange (ch.&.u4)  71           (x.&.u7)
--   assignChannel (SoundRelease' x)        = \ch -> ControlChange (ch.&.u4)  72           (x.&.u7)
--   assignChannel (SoundAttack' x)         = \ch -> ControlChange (ch.&.u4)  73           (x.&.u7)
--   assignChannel (SoundCutoff' x)         = \ch -> ControlChange (ch.&.u4)  74           (x.&.u7)
--   assignChannel (AllSoundsOff')          = \ch -> ControlChange (ch.&.u4) 120           0x00
--   assignChannel (ResetAllControllers')   = \ch -> ControlChange (ch.&.u4) 121           0x00
--   assignChannel (LocalControl' x)        = \ch -> ControlChange (ch.&.u4) 122           (if x then 127 else 0)
--   assignChannel (AllNotesOff')           = \ch -> ControlChange (ch.&.u4) 123           0x00
--   assignChannel (OmniOff')               = \ch -> ControlChange (ch.&.u4) 124           0x00
--   assignChannel (OmniOn')                = \ch -> ControlChange (ch.&.u4) 125           0x00
--   assignChannel (MonoModeN' x)           = \ch -> ControlChange (ch.&.u4) 126           (x.&.u7)
--   assignChannel (PolyMode')              = \ch -> ControlChange (ch.&.u4) 127           0x00


-- NOTATKI

-- b x  = send (PitchBend' x)
-- o    = send AllNotesOff'
-- oo   = sequence_ [loc (env[ch:=n] >> o) | n <- [0..15]]
-- pc x = send (ProgramChange' x)

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



