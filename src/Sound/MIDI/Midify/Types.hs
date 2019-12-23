module Sound.MIDI.Midify.Types  where

import Codec.Midi (Track, Message)
import Foreign.C  (CULong)

-- |PC clock time.
type PCClock = CULong

-- |Time expressed in beats.
type TrackTime = Double

-- |Midi event type
type Event = (TrackTime,Message)

-- |Midi track type.
type MidiTrack = Track TrackTime
