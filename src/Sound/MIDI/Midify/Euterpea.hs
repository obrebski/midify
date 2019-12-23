{-# LANGUAGE FlexibleInstances #-}

module Sound.MIDI.Midify.Euterpea ( module Euterpea, module Sound.MIDI.Midify.Euterpea )

where

import Sound.MIDI.Midify
import Euterpea

instance Midifiable (Music Pitch) where
  send (Prim (Note dur pitch))   = see vel >>= send . NoteOn' (absPitch pitch) >>
                                   pause dur >>
                                   see vel' >>= send . NoteOff' (absPitch pitch)
  send (Prim (Rest dur))         = pause dur
  send (m1 :+: m2)               = send m1 >> send m2
  send (m1 :=: m2)               = gettime >>= \t -> send m1 >> gettime >>= \t'  ->
                                   settime t      >> send m2 >> gettime >>= \t'' ->
                                   settime (max t' t'')
--  send (Modify (Tempo x) m)      = loc $ env[bpm:~(* x)] >> send m
--  send (Modify (Transpose x) m)  = loc $ env[tra:=x] >> send m
  send (Modify (Instrument x) m) = let p = fromEnum x in loc $ send (ProgramChange' p) >> send m
