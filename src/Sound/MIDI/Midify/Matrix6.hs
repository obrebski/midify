module Sound.MIDI.Midify.Matrix6 ( module Sound.MIDI.Midify.Matrix6
                                 , module Sound.MIDI.Midify
                                 , module Codec.Midi
                                 ) where

import Euterpea -- ????????
import Sound.MIDI.Midify
import Codec.Midi (Message(..))
import Data.Word
import Data.ByteString.Lazy (pack)

type Byte = Word8

type ZeroTo63        = Int
zeroTo63 x           = x >= 0 && x <= 63

type ZeroTo127       = Int
zeroTo127 x          = x >= 0 && x <= 127

type Minus31ToPlus31 = Int
minus31ToPlus31 x    = x >= (-31) && x <= 31

type Minus63ToPlus63 = Int
minus63ToPlus63 x    = x >= (-63) && x <= 63

type PatchNumber     = Int -- 0-99

instance Midifiable Operation where
  send (SinglePatchRequest p)      = send $ mkSysex 0x00 [fromIntegral p] False
  send EnterRemoteEditMode         = send $ mkSysex 0x05 [] False
  send (ChangeParameter p)         = send $ mkSysex 0x06 (toBytes p) False

instance Midifiable Parameter where
  send = send . ChangeParameter


mkSysex :: Byte -> [Byte] -> Bool -> Message
mkSysex opcode bytes withcc = Sysex 0 $ pack $ contents
  where
    contents = [0xF0, 0x10, 0x06, opcode]
               ++ bytes
               ++ (if withcc then [checksum bytes] else [])
               ++ [0xF7]

checksum bytes = 0

data Operation = SinglePatchRequest PatchNumber
               | SinglePatchData PatchNumber [Byte]
               | SplitPatchData PatchNumber [Byte]
               | MasterParameterData [Byte]
               | GeneralDataRequest Int PatchNumber
               | EnterRemoteEditMode
               | ChangeParameter Parameter                -- 2 bytes
               deriving (Show)


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

data Parameter = DCO1Freq           ZeroTo63          -- [ 0]   0 -  63 (semitones)
               | DCO1FreqModByLFO   Minus63ToPlus63   -- [ 1] -63 - +63
               | DCO1Sync           SyncType          -- [ 2]   0 -   3 (none,soft,medium,hard)
               | DCO1PW             ZeroTo63          -- [ 3]   0 -  63 (31 - square)
               | DCO1PWMByLFO2      Minus63ToPlus63   -- [ 4] -63 - +63
               | DCO1WaveShape      ZeroTo63          -- [ 5]   0 -  63 (0-triangle, 63-saw)
               | DCO1Wave           WaveType          -- [ 6]   0 -   3 (off,pulse,wave,both)
               | DCO1Levers         LeversType        -- [ 7]   0 -   3 (off,bend,vib,both)
               | DCO1KeybPort       KeybPortType      -- [ 8]   0 -   1 (porta,keyb)
               | DCO1Click          SwitchOnOff       -- [ 9]   0 -   1 (off,on)
               | DCO2Freq           ZeroTo63          -- [10]   0 -  63 (semitones)
               | DCO2FreqModByLFO   Minus63ToPlus63   -- [11] -63 - +63
               | DCO2Detune         Minus31ToPlus31   -- [12] -31 - +31 (+/- a 1/4 tone)
               | DCO2PW             ZeroTo63          -- [13]   0 -  63 (31 - square)
               | DCO2PWMByLFO2      Minus63ToPlus63   -- [14] -63 - +63
               | DCO2WaveShape      ZeroTo63          -- [15]   0 -  63 (0-triangle, 63-saw)
               | DCO2Wave           WaveType          -- [16]   0 -   3 (off,pulse,wave,both,noise)
               | DCO2Levers         LeversType        -- [17]   0 -   3 (off,bend,vib,both)
               | DCO2KeybPort       KeybPortType      -- [18]   0 -   2 (porta,keyb,off)
               | DCO2Click          SwitchOnOff       -- [19]   0 -   1 (off,on)
               | VCFBalance         ZeroTo63          -- [20]   0 -  63 (31 - equal)
               | VCFFrequency       ZeroTo127         -- [21]   0 - 127
               | VCFFreqModByEnv1   Minus63ToPlus63   -- [22] -63 - +63
               | VCFFreqModByPress  Minus63ToPlus63   -- [23] -63 - +63
               | VCFResonance       ZeroTo63          -- [24]   0 -  63
               | VCFLevers          LeversType        -- [25]   0 -   3 (off,bend,vib,both)
               | VCFKeybPort        KeybPortType      -- [26]   0 -   2 (porta,keyb,off)
               | VCA1Volume         ZeroTo63          -- [27]   0 -  63
               | VCA1ModByVel       Minus63ToPlus63   -- [28] -63 - +63
               | VCA2ModByEnv2      Minus63ToPlus63   -- [29] -63 - +63
               | FMAmount           ZeroTo63          -- [30]   0 -  63
               | FMModByEnv3        Minus63ToPlus63   -- [31] -63 - +63
               | FMModByPres        Minus63ToPlus63   -- [32] -63 - +63
               | TrackInput         ModSource         -- [33]   0 -  20 (modulation sources)
               | TrackPoint1        ZeroTo63          -- [34]   0 -  63 (def. 0)
               | TrackPoint2        ZeroTo63          -- [35]   0 -  63 (def. 15)
               | TrackPoint3        ZeroTo63          -- [36]   0 -  63 (def. 31)
               | TrackPoint4        ZeroTo63          -- [37]   0 -  63 (def. 47)
               | TrackPoint5        ZeroTo63          -- [38]   0 -  63 (def. 63)
               | Ramp1Rate          ZeroTo63          -- [40]   0 -  63
               | Ramp1Trigger       RampTrigType      -- [41]   0 -   3 (single,multi,ext,extmulti)
               | Ramp2Rate          ZeroTo63          -- [42]   0 -  63
               | Ramp2Trigger       RampTrigType      -- [43]   0 -   3 (single,multi,ext,extmulti)
               | PortaRate          ZeroTo63          -- [44]   0 -  63
               | PortaModByVel      Minus63ToPlus63   -- [45] -63 - +63
               | PortaMode          PortaType         -- [46]   0 -   2 (linear, const, expo)
               | LegatoPort         SwitchOnOff       -- [47]   0 -   1 (off, on)
               | KeybMode           KeybModeType      -- [48]   0 -   ? (reasign,rotate,unison,reassign rob,rotate rob) ???
               | Env1Delay          ZeroTo63          -- [50]   0 -  63
               | Env1Attack         ZeroTo63          -- [51]   0 -  63
               | Env1Decay          ZeroTo63          -- [52]   0 -  63
               | Env1Sustain        ZeroTo63          -- [53]   0 -  63
               | Env1Release        ZeroTo63          -- [54]   0 -  63
               | Env1Amp            ZeroTo63          -- [55]   0 -  63
               | Env1AmpModByVel    ZeroTo63          -- [56]   0 -  63
               | Env1TrigMode       EnvTrigType       -- [57]   0 -   7
               | Env1Mode           EnvModeType       -- [58]   0 -   3 (normal, dadr, free, freedadr)
               | Env1LFO1Trig       LFOTrigType       -- [59]   0 -   2
               | Env2Delay          ZeroTo63          -- [60]   0 -  63
               | Env2Attack         ZeroTo63          -- [61]   0 -  63
               | Env2Decay          ZeroTo63          -- [62]   0 -  63
               | Env2Sustain        ZeroTo63          -- [63]   0 -  63
               | Env2Release        ZeroTo63          -- [64]   0 -  63
               | Env2Amp            ZeroTo63          -- [65]   0 -  63
               | Env2AmpModByVel    ZeroTo63          -- [66]   0 -  63
               | Env2TrigMode       EnvTrigType       -- [67]   0 -   7
               | Env2Mode           EnvModeType       -- [68]   0 -   3 (normal, dadr, free, freedadr)
               | Env2LFO1Trig       LFOTrigType       -- [69]   0 -   2
               | Env3Delay          ZeroTo63          -- [70]   0 -  63
               | Env3Attack         ZeroTo63          -- [71]   0 -  63
               | Env3Decay          ZeroTo63          -- [72]   0 -  63
               | Env3Sustain        ZeroTo63          -- [73]   0 -  63
               | Env3Release        ZeroTo63          -- [74]   0 -  63
               | Env3Amp            ZeroTo63          -- [75]   0 -  63
               | Env3AmpModByVel    ZeroTo63          -- [76]   0 -  63
               | Env3TrigMode       EnvTrigType       -- [77]   0 -   7
               | Env3Mode           EnvModeType       -- [78]   0 -   3 (normal, dadr, free, freedadr)
               | Env3LFO1Trig       LFOTrigType       -- [79]   0 -   2
               | LFO1Speed          ZeroTo63          -- [80]   0 -  63
               | LFO1SpeedModByPres Minus63ToPlus63   -- [81] -63 - +63
               | LFO1Wave           LFOWaveType       -- [82]   0 -   6
               | LFO1RetrigPoint    ZeroTo63          -- [83]   0 -  63 (0 - normal, 63 - half cycle)
               | LFO1Amplitude      ZeroTo63          -- [84]   0 -  63
               | LFO1AmpModByRamp1  Minus63ToPlus63   -- [85] -63 - +63
               | LFO1TrigMode       LFOTrigModeType   -- [86]   0 -   3 (off, single, multi, extern)
               | LFO1Lag            SwitchOnOff       -- [87]   0 -   1 (off, on)
               | LFO1SampleSrc      ModSource         -- [88]   0 -  20 (modulation sources)
               | LFO2Speed          ZeroTo63          -- [90]   0 -  63
               | LFO2SpeedModByKeyb Minus63ToPlus63   -- [91] -63 - +63
               | LFO2Wave           LFOWaveType       -- [92]   0 -   6
               | LFO2RetrigPoint    ZeroTo63          -- [93]   0 -  63 (0 - normal, 63 - half cycle)
               | LFO2Amplitude      ZeroTo63          -- [94]   0 -  63
               | LFO2AmpModByRamp2 Minus63ToPlus63    -- [95] -63 - +63
               | LFO2TrigMode       LFOTrigModeType   -- [96]   0 -   3 (off, single, multi, extern)
               | LFO2Lag            SwitchOnOff       -- [97]   0 -   1 (off, on)
               | LFO2SampleSrc      ModSource         -- [98]   0 -  20 (modulation sources)
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
toBytes (DCO1Freq x)           = [0,  fromZeroTo63 x]
toBytes (DCO1FreqModByLFO x)   = [1,  fromMinus63ToPlus63 x]
toBytes (DCO1Sync x)           = [2,  fromValueType x]
toBytes (DCO1PW x)             = [3,  fromZeroTo63 x]
toBytes (DCO1PWMByLFO2 x)      = [4,  fromMinus63ToPlus63 x]
toBytes (DCO1WaveShape x)      = [5,  fromZeroTo63 x]
toBytes (DCO1Wave x)           = [6,  fromValueType x]
toBytes (DCO1Levers x)         = [7,  fromValueType x]
toBytes (DCO1KeybPort x)       = [8,  fromValueType x]
toBytes (DCO1Click x)          = [9,  fromValueType x]
toBytes (DCO2Freq x)           = [10, fromZeroTo63 x]
toBytes (DCO2FreqModByLFO x)   = [11, fromMinus63ToPlus63 x]
toBytes (DCO2Detune x)         = [12, fromMinus31ToPlus31 x]
toBytes (DCO2PW x)             = [13, fromZeroTo63 x]
toBytes (DCO2PWMByLFO2 x)      = [14, fromMinus63ToPlus63 x]
toBytes (DCO2WaveShape x)      = [15, fromZeroTo63 x]
toBytes (DCO2Wave x)           = [16, fromValueType x]
toBytes (DCO2Levers x)         = [17, fromValueType x]
toBytes (DCO2KeybPort x)       = [18, fromValueType x]
toBytes (DCO2Click x)          = [19, fromValueType x]
toBytes (VCFBalance x)         = [20, fromZeroTo63 x]
toBytes (VCFFrequency x)       = [21, fromZeroTo127 x]
toBytes (VCFFreqModByEnv1 x)   = [22, fromMinus63ToPlus63 x]
toBytes (VCFFreqModByPress x)  = [23, fromMinus63ToPlus63 x]
toBytes (VCFResonance x)       = [24, fromZeroTo63 x]
toBytes (VCFLevers x)          = [25, fromValueType x]
toBytes (VCFKeybPort x)        = [26, fromValueType x]
toBytes (VCA1Volume x)         = [27, fromZeroTo63 x]
toBytes (VCA1ModByVel x)       = [28, fromMinus63ToPlus63 x]
toBytes (VCA2ModByEnv2 x)      = [29, fromMinus63ToPlus63 x]
toBytes (FMAmount x)           = [30, fromZeroTo63 x]
toBytes (FMModByEnv3 x)        = [31, fromMinus63ToPlus63 x]
toBytes (FMModByPres x)        = [32, fromMinus63ToPlus63 x]
toBytes (TrackInput x)         = [33, fromValueType x]
toBytes (TrackPoint1 x)        = [34, fromZeroTo63 x]
toBytes (TrackPoint2 x)        = [35, fromZeroTo63 x]
toBytes (TrackPoint3 x)        = [36, fromZeroTo63 x]
toBytes (TrackPoint4 x)        = [37, fromZeroTo63 x]
toBytes (TrackPoint5 x)        = [38, fromZeroTo63 x]
toBytes (Ramp1Rate x)          = [40, fromZeroTo63 x]
toBytes (Ramp1Trigger x)       = [41, fromValueType x]
toBytes (Ramp2Rate x)          = [41, fromZeroTo63 x]
toBytes (Ramp2Trigger x)       = [43, fromValueType x]
toBytes (PortaRate x)          = [44, fromZeroTo63 x]
toBytes (PortaModByVel x)      = [45, fromMinus63ToPlus63 x]
toBytes (PortaMode x)          = [46, fromValueType x]
toBytes (LegatoPort x)         = [47, fromValueType x]
toBytes (KeybMode x)           = [48, fromValueType x]
toBytes (Env1Delay x)          = [50, fromZeroTo63 x]
toBytes (Env1Attack x)         = [51, fromZeroTo63 x]
toBytes (Env1Decay x)          = [52, fromZeroTo63 x]
toBytes (Env1Sustain x)        = [53, fromZeroTo63 x]
toBytes (Env1Release x)        = [54, fromZeroTo63 x]
toBytes (Env1Amp x)            = [55, fromZeroTo63 x]
toBytes (Env1AmpModByVel x)    = [56, fromZeroTo63 x]
toBytes (Env1TrigMode x)       = [57, fromValueType x]
toBytes (Env1Mode x)           = [58, fromValueType x]
toBytes (Env1LFO1Trig x)       = [59, fromValueType x]
toBytes (Env2Delay x)          = [60, fromZeroTo63 x]
toBytes (Env2Attack x)         = [61, fromZeroTo63 x]
toBytes (Env2Decay x)          = [62, fromZeroTo63 x]
toBytes (Env2Sustain x)        = [63, fromZeroTo63 x]
toBytes (Env2Release x)        = [64, fromZeroTo63 x]
toBytes (Env2Amp x)            = [65, fromZeroTo63 x]
toBytes (Env2AmpModByVel x)    = [66, fromZeroTo63 x]
toBytes (Env2TrigMode x)       = [67, fromValueType x]
toBytes (Env2Mode x)           = [68, fromValueType x]
toBytes (Env2LFO1Trig x)       = [69, fromValueType x]
toBytes (Env3Delay x)          = [70, fromZeroTo63 x]
toBytes (Env3Attack x)         = [71, fromZeroTo63 x]
toBytes (Env3Decay x)          = [72, fromZeroTo63 x]
toBytes (Env3Sustain x)        = [73, fromZeroTo63 x]
toBytes (Env3Release x)        = [74, fromZeroTo63 x]
toBytes (Env3Amp x)            = [75, fromZeroTo63 x]
toBytes (Env3AmpModByVel x)    = [76, fromZeroTo63 x]
toBytes (Env3TrigMode x)       = [77, fromValueType x]
toBytes (Env3Mode x)           = [78, fromValueType x]
toBytes (Env3LFO1Trig x)       = [79, fromValueType x]
toBytes (LFO1Speed x)          = [80, fromZeroTo63 x]
toBytes (LFO1SpeedModByPres x) = [81, fromMinus63ToPlus63 x]
toBytes (LFO1Wave x)           = [82, fromValueType x]
toBytes (LFO1RetrigPoint x)    = [83, fromZeroTo63 x]
toBytes (LFO1Amplitude x)      = [84, fromZeroTo63 x]
toBytes (LFO1AmpModByRamp1 x)  = [85, fromMinus63ToPlus63 x]
toBytes (LFO1TrigMode x)       = [86, fromValueType x]
toBytes (LFO1Lag x)            = [87, fromValueType x]
toBytes (LFO1SampleSrc x)      = [88, fromValueType x]
toBytes (LFO2Speed x)          = [90, fromZeroTo63 x]
toBytes (LFO2SpeedModByKeyb x) = [91, fromMinus63ToPlus63 x]
toBytes (LFO2Wave x)           = [92, fromValueType x]
toBytes (LFO2RetrigPoint x)    = [93, fromZeroTo63 x]
toBytes (LFO2Amplitude x)      = [94, fromZeroTo63 x]
toBytes (LFO2AmpModByRamp2 x)  = [95, fromMinus63ToPlus63 x]
toBytes (LFO2TrigMode x)       = [96, fromValueType x]
toBytes (LFO2Lag x)            = [97, fromValueType x]
toBytes (LFO2SampleSrc x)      = [98, fromValueType x]
