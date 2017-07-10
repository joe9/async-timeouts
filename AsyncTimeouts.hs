{-# LANGUAGE DeriveDataTypeable #-}

module AsyncTimeouts
  ( waitTimeout
  , waitTimeoutInSeconds
  , asyncTimeout
  , asyncTimeoutInSeconds
  ) where

-- got this from https://gist.github.com/robstewart57/4195051
import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (Async, race, wait, withAsync)
import Control.Exception        (Exception, throw)
import Control.Exception.Safe   (throwM)
import Control.Monad            (void)
import Data.Typeable            (Typeable)

import ThreadDelayInSeconds (threadDelayInSeconds)

data ThreadTimeoutException =
  ThreadTimeoutException
  deriving (Show, Typeable)

instance Exception ThreadTimeoutException

waitTimeout :: Async a -> Int -> IO (Either ThreadTimeoutException a)
waitTimeout t i = race (threadDelay i >> throw ThreadTimeoutException) (wait t)

waitTimeoutInSeconds :: Async a -> Int -> IO (Either ThreadTimeoutException a)
waitTimeoutInSeconds t i =
  race (threadDelayInSeconds i >> throw ThreadTimeoutException) (wait t)

asyncTimeout :: IO a -> Int -> IO ()
asyncTimeout f i =
  withAsync f $ \a1 ->
    withAsync (threadDelay i) $ \a2 -> void $ race (wait a1) (wait a2)

asyncTimeoutInSeconds :: IO b -> Int -> IO b
asyncTimeoutInSeconds f i = do
  er <- race (threadDelayInSeconds i) f
  case er of
    Left _  -> throwM ThreadTimeoutException
    Right r -> return r
