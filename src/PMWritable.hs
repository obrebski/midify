{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module PMWritable (initialize,terminate, module PMWritable)
where

import Codec.Midi
import Sound.PortMidi (
                        PMEvent(PMEvent), PMMsg(PMMsg),
                        PMStream,
                        PMError(..),
                        DeviceID,
                        initialize, terminate,
                        openInput, openOutput, close,
                        writeEvents, writeShort, writeSysEx,
                        time,
                        encodeMsg
                      )
import Foreign.C (CLong (CLong),CULong (CULong))
import Data.Bits ((.|.),(.&.),shiftR)
import Data.ByteString.Lazy (unpack)
import Data.Word (Word32)

type PCClock  = Word32

class PMWritable a where
  write :: PMStream -> a -> IO PMError

instance PMWritable (PCClock, Message) where
  write str (t,msg) | isChannelMessage msg = time >>= \now -> writeShort str $ PMEvent (encodeMsg $ toPMMsg msg) (now + (CULong t))
  write str (t,Sysex n bytes)              = time >>= \now -> writeSysEx str (now + (CULong t)) $ map (toEnum . fromEnum) $ unpack bytes
  write _   _                              = return BadData

instance PMWritable Message where
  write str msg = write str (0::PCClock,msg)

instance PMWritable (Track PCClock) where
  write _ [] = return NoError
  write str (event:track) = write str event >>= \case
                                                   NoError -> write str track
                                                   error   -> return error

toPMMsg :: Message -> PMMsg
toPMMsg (NoteOff c p v)         = PMMsg (128 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
toPMMsg (NoteOn c p v)          = PMMsg (144 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
toPMMsg (KeyPressure c p pr)    = PMMsg (160 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral pr)
toPMMsg (ControlChange c cn cv) = PMMsg (176 .|. (fromIntegral c .&. 0xF)) (fromIntegral cn) (fromIntegral cv)
toPMMsg (ProgramChange c pn)    = PMMsg (192 .|. (fromIntegral c .&. 0xF)) (fromIntegral pn) 0
toPMMsg (ChannelPressure c pr)  = PMMsg (208 .|. (fromIntegral c .&. 0xF)) (fromIntegral pr) 0
toPMMsg (PitchWheel c pb)       = PMMsg (224 .|. (fromIntegral c .&. 0xF)) (fromIntegral lo) (fromIntegral hi)
  where (hi,lo) = (pb `shiftR` 8, pb .&. 0xFF)

openMidiOutput :: DeviceID -> IO PMStream
openMidiOutput dev = initialize >> openOutput dev 10 >>= \case
                                                            Left stream -> return stream
                                                            Right err   -> error (show err)

start n = initialize >> openMidiOutput 2
