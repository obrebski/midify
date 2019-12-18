{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Sound.MIDI.Midify.PMWritable ( initialize
                                    , terminate
                                    , PMError
                                    , module Sound.MIDI.Midify.PMWritable
                                    ) where

import Codec.Midi     ( Message(..), Track, isChannelMessage )
import Sound.PortMidi ( PMEvent(PMEvent), PMMsg(PMMsg), PMStream, PMError(..), PMSuccess(..)
                      , DeviceID
                      , initialize, terminate, openInput, openOutput, close
                      , writeEvents, writeShort, writeSysEx
                      , time
                      , encodeMsg
                      )
import Foreign.C      ( CULong )
import Data.Bits      ( (.|.), (.&.), shiftR )

import Data.ByteString.Lazy ( unpack )


writeMsg :: PMStream -> (Timestamp,Message) -> IO (Either PMError PMSuccess)
writeMsg str (t,msg) | isChannelMessage msg = writeShort str $ PMEvent (encodeMsg $ toPMMsg msg) t
writeMsg str (t,Sysex n bytes)              = writeSysEx str t $ map (toEnum . fromEnum) $ unpack bytes
writeMsg _   _                              = return $ Left BadData


type Timestamp  = CULong

class PMWritable a where
  write :: PMStream -> a -> IO (Either PMError PMSuccess)

instance PMWritable (Timestamp, Message) where
  write str (t,msg) | isChannelMessage msg = time >>= \now ->
                                                    writeShort str $ PMEvent (encodeMsg $ toPMMsg msg) (now + t)
  write str (t,Sysex n bytes)              = time >>= \now -> writeSysEx str (now + t) $ map (toEnum . fromEnum) $ unpack bytes
  write _   _                              = return $ Left BadData

instance PMWritable Message where
  write str msg = write str (0::Timestamp,msg)

instance PMWritable (Track Timestamp) where
  write _ [] = return $ Right GotData
  write str (event:track) = write str event >>= \case
                                                   Right _      -> write str track
                                                   Left error   -> return $ Left error

toPMMsg :: Message -> PMMsg
toPMMsg = \case
             NoteOff c p v         -> mkPMMsg 128 c p v
             NoteOn c p v          -> mkPMMsg 144 c p v
             KeyPressure c p pr    -> mkPMMsg 160 c p pr
             ControlChange c cn cv -> mkPMMsg 176 c cn cv
             ProgramChange c pn    -> mkPMMsg 192 c pn 0
             ChannelPressure c pr  -> mkPMMsg 208 c pr 0
             PitchWheel c pb       -> let (hi,lo) = (pb `shiftR` 8, pb .&. 0xFF) in mkPMMsg 224 c lo hi
             where
               mkPMMsg a b c d = PMMsg (a .|. (fromIntegral b .&. 0xF)) (fromIntegral c) (fromIntegral d)

openMidiOutput :: DeviceID -> IO PMStream
openMidiOutput dev = initialize >> openOutput dev 10 >>= \case
                                                           Right stream -> return stream
                                                           Left err   -> error (show err)

start :: Int -> IO PMStream
start n = initialize >> openMidiOutput n
