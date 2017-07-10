{-# LANGUAGE DeriveDataTypeable #-}

module AsyncTimeouts
  ( waitTimeout
  , asyncTimeout
  , asyncTimeoutInSeconds
  ) where

-- got this from https://gist.github.com/robstewart57/4195051
import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (Async, race, wait, withAsync)
import Control.Exception        (Exception, throw)
import Control.Monad            (void)
import Data.Typeable            (Typeable)

import ThreadDelayInSeconds (threadDelayInSeconds)

data ThreadTimeoutException =
  ThreadTimeoutException
  deriving (Show, Typeable)

instance Exception ThreadTimeoutException

waitTimeout :: Async a -> Int -> IO (Either ThreadTimeoutException a)
waitTimeout t i = race (threadDelay i >> throw ThreadTimeoutException) (wait t)

asyncTimeout :: IO a -> Int -> IO ()
asyncTimeout f i =
  withAsync f $ \a1 ->
    withAsync (threadDelay i) $ \a2 -> void $ race (wait a1) (wait a2)

asyncTimeoutInSeconds :: IO a -> Int -> IO ()
asyncTimeoutInSeconds f i =
  withAsync f $ \a1 ->
    withAsync (threadDelayInSeconds i) $ \a2 -> void $ race (wait a1) (wait a2)
