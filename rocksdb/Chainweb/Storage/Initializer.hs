{-# language CPP #-}
{-# language ForeignFunctionInterface #-}
{-# language EmptyDataDecls #-}
{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language GADTs #-}

module Chainweb.Storage.Initializer where

import Control.Monad
import Control.Monad.Catch
import Data.ByteArray
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Foreign.Concurrent as FC
import qualified GHC.Foreign as GHC
import qualified GHC.IO.Encoding as GHC
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO.Temp

data Initializer o = forall i. Initializer
    { setupInitializer :: IO i
    , applyInitializer :: i -> Ptr o -> IO ()
    , initializerCleanup :: i -> IO ()
    }

simpleInitializer :: (Ptr o -> IO ()) -> Initializer o
simpleInitializer apply =
    Initializer (return ()) (\_ -> apply) (\_ -> return ())

instance Monoid (Initializer o) where
    mempty = Initializer (return ()) (\_ _ -> return ()) (\_ -> return ())
instance Semigroup (Initializer o) where
    Initializer s a c <> Initializer s' a' c' = Initializer
        { setupInitializer = (,) <$> s <*> s'
        , applyInitializer = \(i1,i2) -> a i1 >> a' i2
        , initializerCleanup = \(i1, i2) -> c' i2 `finally` c i1
        }

allocateInitializer :: IO (Ptr p) -> (Ptr p -> IO ()) -> Initializer p -> IO (ForeignPtr p)
allocateInitializer mkP cleanupP (Initializer setup apply cleanup) = do
    i <- setup
    (p, clean) <- flip onException (cleanup i) $ do
        p <- mkP
        flip onException (cleanupP p) $ do
            apply i p
            return (p, cleanupP p >> cleanup i)
    FC.newForeignPtr p clean

withInitializer :: IO (Ptr p) -> (Ptr p -> IO ()) -> Initializer p -> (Ptr p -> IO a) -> IO a
withInitializer mkP cleanupP r act =
    bracket (allocateInitializer mkP cleanupP r) finalizeForeignPtr $ flip withForeignPtr act