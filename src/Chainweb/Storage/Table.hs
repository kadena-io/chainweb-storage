{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language FunctionalDependencies #-}

module Chainweb.Storage.Table
    ( Table(..)
    , HasKey(..)
    , casWrite
    , casMultiWrite
    , Iterator(..)
    , withIter
    , iterKeys
    , iterValues
    , iterEntries
    , Entry(..)
    )
    where

import Control.Exception
import System.IO.Unsafe(unsafeInterleaveIO)

class Iterator i => Table t i | t -> i where
    singleRead :: k -> t k v -> IO (Maybe v)
    multiRead :: [k] -> t k v -> IO [Maybe v]
    singleWrite :: k -> v -> t k v -> IO ()
    multiWrite :: [(k, v)] -> t k v -> IO ()
    singleDelete :: k -> t k v -> IO ()
    multiDelete :: [k] -> t k v -> IO ()
    createIter :: t k v -> IO (i k v)
    -- ^ must be positioned at the start of the table
    -- must also refer to a "version" of the table, ignoring concurrent writes

withIter :: Table t i => t k v -> (i k v -> IO a) -> IO a
withIter t = bracket (createIter t) destroyIter

data Entry k v = Entry !k !v

class Iterator i where
    iterValid :: i k v -> IO Bool
    iterNext :: i k v -> IO ()
    iterPrev :: i k v -> IO ()
    iterFirst :: i k v -> IO ()
    iterLast :: i k v -> IO ()
    iterSeek :: i k v -> k -> IO ()
    iterEntry :: i k v -> IO (Entry k v)
    iterKey :: i k v -> IO k
    iterValue :: i k v -> IO v
    destroyIter :: i k v -> IO ()

class HasKey v k | v -> k where
    getKey :: v -> k

casWrite :: (HasKey v k, Table t i) => v -> t k v -> IO ()
casWrite v t = singleWrite (getKey v) v t

casMultiWrite :: (HasKey v k, Table t i) => [v] -> t k v -> IO ()
casMultiWrite vs t = multiWrite [ (getKey v, v) | v <- vs ] t

iterParts :: Iterator i => i k v -> IO a -> IO [a]
iterParts it acc = go
    where
    go = unsafeInterleaveIO $ do
        v <- iterValid it
        if v
        then (:) <$> acc <*> go
        else return []

iterEntries :: Iterator i => i k v -> IO [Entry k v]
iterEntries it = iterParts it (iterEntry it)

iterKeys :: Iterator i => i k v -> IO [k]
iterKeys it = iterParts it (iterKey it)

iterValues :: Iterator i => i k v -> IO [v]
iterValues it = iterParts it (iterValue it)
