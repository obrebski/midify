module Shephard (m6Setup,up,down,tone,tones) where

import Euterpea (AbsPitch,Dur)
import Matrix6

pa = pause 0.1

up = tones 30 102 75 13 6

down = tones 102 30 75 13 6

m6Setup =  env[ch:=1] >> pa >>
           send EnterRemoteEditMode >> pa >>
           send (ProgramChange' 33) >> pa >>
           send MonoMode' >> pa >>
           send (PortaMode CONST) >> pa >>
           send (PortaRate 58) >> pa >>
           send (LegatoPort ON) >> pa >>
           send (DCO1KeybPort PORTAMENTO) >> pa >>
           send (DCO2KeybPort PORTAMENTO) >> pa >>
           send (VCA2ModByEnv2 63) >> pa >>
           send (Env2Attack 50) >> pa >>
           send (Env2Release 45) >> pa >>
           send (VCFFrequency 1) >> pa >>
           send (VCFFreqModByPress 63) >> pa >>
           send (VCFKeybPort PORTAMENTO) >> pa >>
           send (VCFFreqModByEnv1 51) >> pa >>
           send (Env1Attack 63) >> pa >>
           send (Env1Release 55)

tone src dst t = do
               send AllNotesOff'
               pause 0.1
               send (NoteOn' src 50)
               pause 0.1
               send (NoteOn' dst 50)
               pause (t - 0.2)
               send (NoteOff' dst 1)

tones :: AbsPitch -> AbsPitch -> Dur -> Dur -> Int -> T ()
tones src dst t d n = sequence_ [ fork ( pause del >> env[ch:=c] >> tone src dst (t-10) ) | i <- [0..(n-1)],
                                                                                            let del = fromIntegral i * d,
                                                                                            let c = (i `mod` 6) + 1 ]

