{-# LANGUAGE FlexibleInstances #-}

module Sound.MIDI.Midify.Euterpea ( module Sound.MIDI.Midify.Euterpea )

where

import Sound.MIDI.Midify
import Euterpea

instance Midifiable (Music Pitch) where
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
