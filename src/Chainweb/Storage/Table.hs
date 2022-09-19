{-# language TypeFamilies #-}
{-# language RankNTypes #-}

module Chainweb.Storage.Table
    ( Table(..)
    , HasKey(..)
    , CasTable(..)
    , casWrite
    , casRead
    , Iterator(..)
    , withIter
    , iterKeys
    , iterValues
    , iterEntries
    , Entry(..)
    , StorageEngine(..)
    )
    where

import Control.Exception
import Data.ByteString(ByteString)
import System.IO.Unsafe(unsafeInterleaveIO)

newtype StorageEngine c
    = StorageEngine
    { newTable :: forall k v. ByteString -> c k v -> Table k v
    }

data Table k v
    = Table
    { scrounge :: (k -> IO (Maybe v))
    , shove :: (k -> v -> IO ())
    , createIter :: IO (Iterator k v)
    -- ^ must be positioned at the start of the table
    -- must also refer to a "version" of the table, ignoring concurrent writes
    , destroyIter :: Iterator k v -> IO ()
    }

withIter :: Table k v -> (Iterator k v -> IO a) -> IO a
withIter t = bracket (createIter t) (destroyIter t)

data Entry k v = Entry !k !v

data Iterator k v
    = Iterator
    { valid :: IO Bool
    , next :: IO ()
    , prev :: IO ()
    , start :: IO ()
    , end :: IO ()
    , entryHere :: IO (Entry k v)
    , keyHere :: IO k
    , valueHere :: IO v
    }

class HasKey v where
    type Key v
    getKey :: v -> Key v

newtype CasTable v = CasTable { getCasTable :: Table (Key v) v }

casWrite :: HasKey v => CasTable v -> v -> IO ()
casWrite (CasTable t) v = shove t (getKey v) v

casRead :: CasTable v -> Key v -> IO (Maybe v)
casRead (CasTable t) k = scrounge t k

iterParts :: Iterator k v -> IO a -> IO [a]
iterParts it acc = go
    where
    go = unsafeInterleaveIO $ do
        v <- valid it
        if v
        then (:) <$> acc <*> go
        else return []

iterEntries :: Iterator k v -> IO [Entry k v]
iterEntries it = iterParts it (entryHere it)

iterKeys :: Iterator k v -> IO [k]
iterKeys it = iterParts it (keyHere it)

iterValues :: Iterator k v -> IO [v]
iterValues it = iterParts it (valueHere it)
