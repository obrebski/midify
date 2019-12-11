

module Sound.MIDI.Midify.Time (MidiTime(..)
                              ) where


import Control.Lens
import Foreign.C (CULong)

-- |The type for bits-per-minute value.
type BPM         = Rational

-- |Time expressed in PC Clock ticks (miliseconds).
type Timestamp = CULong

-- |Different ways of time interval specification.
data MidiTime
  = B Rational    -- ^Time expressed in quarternotes.
  | R Float       -- ^Time expressed in seconds.

