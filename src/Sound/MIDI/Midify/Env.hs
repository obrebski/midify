{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Sound.MIDI.Midify.Env

where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS

import Codec.Midi
import Sound.MIDI.Midify (BPM,T)
import Control.Lens
import Control.Concurrent

class LensAccess box l where
  aView   :: MonadIO m => Getting a l a             -> box -> m a
  aOver   :: MonadIO m => ASetter l l a a -> (a->a) -> box -> m ()
  aSet    :: MonadIO m => ASetter l l a a -> a      -> box -> m ()
  aSet f = aOver f <$> const
  
data Env = Env { _ch   :: Channel
               , _bpm  :: BPM
               , _vel  :: Velocity
               , _velr :: Velocity
               }

makeLenses ''Env


instance LensAccess Env Env where
  aView f   = return . (^.f)
  aSet  f v = return . const () . set f v 
  aOver f v = return . const () . over f v

instance LensAccess (MVar Env) Env where
  aView f me = liftIO $ readMVar me >>= return . (^.f)
  aSet  f v me = liftIO $ modifyMVar_ me (return . set f v)
  aOver f v me = liftIO $ modifyMVar_ me (return . over f v)
  
instance LensAccess (RWST Bool (Track t) (t,Env) IO ()) Env where
  aView f _ = view (_2.f) <$> get

  
-- instance EnvClass Env where
--   setCh = return . _ch
--   setBpm = return . _bpm
--   setVel = return . _vel
--   setVelr = return . _velr


-- instance EnvClass MEnv where
--   setCh   (MEnv me) = liftIO $ _ch   <$> readMVar me
--   setBpm  (MEnv me) = liftIO $ _bpm  <$> readMVar me
--   setVel  (MEnv me) = liftIO $ _vel  <$> readMVar me
--   setVelr (MEnv me) = liftIO $ _velr <$> readMVar me
