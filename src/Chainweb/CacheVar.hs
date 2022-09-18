{-# language NumericUnderscores #-}
{-# language LambdaCase #-}

module Chainweb.CacheVar where

import Control.Concurrent(threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Time

data CacheVar a = CacheVar !Int !(IO a) !(MVar (Maybe (a, Async ())))

mkCacheVar :: DiffTime -> IO a -> IO (CacheVar a)
mkCacheVar d i = CacheVar (round $ d * 1_000_000) i <$> newMVar Nothing

readCacheVar :: CacheVar a -> IO a
readCacheVar (CacheVar d initialize ref) =
    modifyMVar ref $ \contents -> do
        a <- case contents of
            Just (a, act) ->
                a <$ cancel act
            Nothing ->
                initialize
        expireThread <- async $ do
            threadDelay d
            modifyMVar_ ref (\_ -> return Nothing)
        return (Just (a, expireThread), a)
