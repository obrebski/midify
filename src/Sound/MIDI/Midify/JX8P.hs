module Sound.MIDI.Midify.JX8P ( module Sound.MIDI.Midify.JX8P
                              , module Sound.MIDI.Midify
                              , module Codec.Midi
                              ) where

import Euterpea
import Sound.MIDI.Midify
import Codec.Midi (Message(..))
import Data.Word
import Data.ByteString.Lazy (pack)

type Byte = Word8

instance Midifiable Operation where
  send (IndividualParameter p)         = send $ mkSysex 0 0x36 (toBytes p)

instance Midifiable Parameter where
  send = send . IndividualParameter


mkSysex :: Byte -> Byte -> [Byte] -> Message
mkSysex channel opcode bytes = Sysex 0 $ pack $ contents
  where
    contents = [0xF0, 0x41, opcode, channel, 0x21, 0x20, 0x01]
               ++ bytes
               ++ [0xF7]

data Operation = IndividualParameter Parameter                -- 2 bytes
               deriving (Show)


type ZeroTo63        = Int
zeroTo63 x           = x >= 0 && x <= 63

type ZeroTo127       = Int
zeroTo127 x          = x >= 0 && x <= 127

type Minus31ToPlus31 = Int
minus31ToPlus31 x    = x >= (-31) && x <= 31

type Minus63ToPlus63 = Int
minus63ToPlus63 x    = x >= (-63) && x <= 63

type PatchNumber     = Int -- 0-99

data SwitchOnOff     = OFF | ON
                     deriving (Show,Enum)
data SyncType        = SyncOFF | SyncSOFT | SyncMEDIUM | SyncHARD
                     deriving (Show,Enum)
data WaveType        = WaveOFF | WavePULSE | WaveWAVE | WaveBOTH
                     deriving (Show,Enum)
data LeversType      = LeversOFF | LeversBEND | LeversVIB | LeversBOTH
                     deriving (Show,Enum)
data KeybPortType    = KeybPortOFF | PORTAMENTO | KEYBOARD
                     deriving (Show,Enum)
data ModSource       = DELETE | ENV1 | ENV2 | ENV3 | LFO1 | LFO2 | VIB | RMP1 | RMP2 | KEYB | PORT
                     | TRACK | GATE | VEL | RVEL | PRES | PED1 | PED2 | LEV1 | LEV2 | LEV3
                     deriving (Show,Enum)
data RampTrigType    = RampTrigSINGLE | RampTrigMULTI | RampTrigEXT | RampTrigEXTGATED
                     deriving (Show,Enum)
data PortaType       = LINEAR | CONST | EXPONENTIAL
                     deriving (Show,Enum)
data KeybModeType    = KeybModeREASSIGN | KeybModeROTATE | KeybModeUNISON | KeybModeREASSIGNROB | KeybModeROTATEROB
                     deriving (Show,Enum)
data EnvTrigType     = EnvTrigSGL | EnvTrigSGLRESET | EnvTrigMUL | EnvTrigMULRESET
                     | EnvTrigXSGL | EnvTrigXSGLRESET | EnvTrigXMULTI | EnvTrigXMULRESET
                     deriving (Show,Enum)
data EnvModeType     = EnvModeNORM | EnvModeFREE | EnvModeDADR | EnvModeBOTH
                     deriving (Show,Enum)
data LFOTrigType     = LFOTrigOFF | LFOTrigLFO1 | LFOTrigGLFO1
                     deriving (Show,Enum)
data LFOWaveType     = LFOWaveTRI | LFOWaveUPSAW | LFOWaveDNSAW | LFOWaveSQUARE | LFOWaveRANDOM | LFOWaveNOISE | LFOWaveSAMPLED
                     deriving (Show,Enum)
data LFOTrigModeType = LFOTrigModeOFF | LFOTrigModeSGL | LFOTrigModeMUL | LFOTrigModeEXT
                     deriving (Show,Enum)

data Parameter = DCO1Range          ZeroTo127   -- 0,1,2,3
               | DCO1Wave           ZeroTo127   -- 0,1,2,3
               | DCO1Tune           ZeroTo127
               | DCO1LFOMod         ZeroTo127
               | DCO1EnvMod         ZeroTo127
               | DCO2Range          ZeroTo127   -- 0,1,2,3
               | DCO2Wave           ZeroTo127   -- 0,1,2,3
               | DCO2Crossmod       ZeroTo127
               | DCO2Tune           ZeroTo127
               | DCO2FineTune       ZeroTo127
               | DCO2LFOMod         ZeroTo127
               | DCO2EnvMod         ZeroTo127
               | DCODynamics        ZeroTo127
               | DCOEnvMode         ZeroTo127
               | MixDCO1            ZeroTo127
               | MixDCO2            ZeroTo127
               | MixEnvMod          ZeroTo127
               | MixDynamics        ZeroTo127
               | MixEnvMode         ZeroTo127
               | HPFCutoff          ZeroTo127
               | VCFCutoff          ZeroTo127
               | VCFRes             ZeroTo127
               | VCFLFOMod          ZeroTo127
               | VCFEnvMod          ZeroTo127
               | VCFKeyFollow       ZeroTo127
               | VCFDynamics        ZeroTo127
               | VCFEnvMode         ZeroTo127
               | VCALevel           ZeroTo127
               | VCADynamics        ZeroTo127
               | VCAEnvMode         ZeroTo127
               | Chorus             ZeroTo127
               | LFOWave            ZeroTo127
               | LFODelay           ZeroTo127
               | LFORate            ZeroTo127
               | ENV1Attack         ZeroTo127
               | ENV1Decay          ZeroTo127
               | ENV1Sustain        ZeroTo127
               | ENV1Release        ZeroTo127
               | ENV1KeyFollow      ZeroTo127
               | ENV2Attack         ZeroTo127
               | ENV2Decay          ZeroTo127
               | ENV2Sustain        ZeroTo127
               | ENV2Release        ZeroTo127
               | ENV2KeyFollow      ZeroTo127
               deriving (Show)

fromValueType :: Enum a => a -> Byte
fromValueType = fromIntegral . fromEnum
fromZeroTo63 x        | x >= 0 && x <= 63   = fromIntegral x
                      | otherwise           = 31
fromZeroTo127 x       | x >= 0 && x <= 127  = fromIntegral x
                      | otherwise           = 63
fromMinus63ToPlus63 x | x >= -63 && x <= 63 = fromIntegral x
                      | otherwise           = 0
fromMinus31ToPlus31 x | x >= -31 && x <= 31 = fromIntegral x
                      | otherwise           = 0

toBytes :: Parameter -> [Byte]
toBytes (DCO1Range x)          = [11,  fromZeroTo127 (x*32)]
toBytes (DCO1Wave x)           = [12,  fromZeroTo127 (x*32)]
toBytes (DCO1Tune x)           = [13,  fromZeroTo127 x]
toBytes (DCO1LFOMod x)         = [14,  fromZeroTo127 x]
toBytes (DCO1EnvMod x)         = [15,  fromZeroTo127 x]
toBytes (DCO2Range x)          = [16,  fromZeroTo127 (x*32)]
toBytes (DCO2Wave x)           = [17,  fromZeroTo127 (x*32)]
toBytes (DCO2Crossmod x)       = [18,  fromZeroTo127 (x*32)]
toBytes (DCO2Tune x)           = [19,  fromZeroTo127 x]
toBytes (DCO2FineTune x)       = [20,  fromZeroTo127 x]
toBytes (DCO2LFOMod x)         = [21,  fromZeroTo127 x]
toBytes (DCO2EnvMod x)         = [22,  fromZeroTo127 x]
toBytes (DCODynamics x)        = [26,  fromZeroTo127 (x*32)]
toBytes (DCOEnvMode x)         = [27,  fromZeroTo127 (x*32)]
toBytes (MixDCO1 x)            = [28,  fromZeroTo127 x]
toBytes (MixDCO2 x)            = [29,  fromZeroTo127 x]
toBytes (MixEnvMod x)          = [30,  fromZeroTo127 x]
toBytes (MixDynamics x)        = [31,  fromZeroTo127 (x*32)]
toBytes (MixEnvMode x)         = [32,  fromZeroTo127 (x*32)]
toBytes (HPFCutoff x)          = [33,  fromZeroTo127 (x*32)]
toBytes (VCFCutoff x)          = [34,  fromZeroTo127 x]
toBytes (VCFRes x)             = [35,  fromZeroTo127 x]
toBytes (VCFLFOMod x)          = [36,  fromZeroTo127 x]
toBytes (VCFEnvMod x)          = [37,  fromZeroTo127 x]
toBytes (VCFKeyFollow x)       = [38,  fromZeroTo127 x]
toBytes (VCFDynamics x)        = [39,  fromZeroTo127 (x*32)]
toBytes (VCFEnvMode x)         = [40,  fromZeroTo127 (x*32)]
toBytes (VCALevel x)           = [41,  fromZeroTo127 x]
toBytes (VCADynamics x)        = [42,  fromZeroTo127 (x*32)]
toBytes (VCAEnvMode x)         = [58,  fromZeroTo127 (x*32)]
toBytes (Chorus x)             = [43,  fromZeroTo127 (x*32)]
toBytes (LFOWave x)            = [44,  fromZeroTo127 (x*32)]
toBytes (LFODelay x)           = [45,  fromZeroTo127 x]
toBytes (LFORate x)            = [46,  fromZeroTo127 x]
toBytes (ENV1Attack x)         = [47,  fromZeroTo127 x]
toBytes (ENV1Decay x)          = [48,  fromZeroTo127 x]
toBytes (ENV1Sustain x)        = [49,  fromZeroTo127 x]
toBytes (ENV1Release x)        = [50,  fromZeroTo127 x]
toBytes (ENV1KeyFollow x)      = [51,  fromZeroTo127 (x*32)]
toBytes (ENV2Attack x)         = [52,  fromZeroTo127 x]
toBytes (ENV2Decay x)          = [53,  fromZeroTo127 x]
toBytes (ENV2Sustain x)        = [54,  fromZeroTo127 x]
toBytes (ENV2Release x)        = [55,  fromZeroTo127 x]
toBytes (ENV2KeyFollow x)      = [56,  fromZeroTo127 x]
