module Chainweb.Storage.Table where

data Table k v
    = Table (k -> Maybe v) (k -> v -> IO ()) (forall a. (Iterator k v -> IO a) -> IO a)

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

keyHere :: Iterator k v -> IO k
keyHere it = do
    Entry k _ <- entryHere it
    return k

valueHere :: Iterator k v -> IO k
valueHere it = do
    Entry _ v <- entryHere it
    return v

