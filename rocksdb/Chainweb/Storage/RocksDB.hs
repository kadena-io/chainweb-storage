{-# language CPP #-}
{-# language ForeignFunctionInterface #-}
{-# language EmptyDataDecls #-}
{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
{-# language GADTs #-}

module Chainweb.Storage.RocksDB
    (

    )
    where

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

import Chainweb.Storage.Initializer
import Chainweb.Storage.Table

data WriteBatch
data ReadOptions
data WriteOptions
data PrefixExtractor
data Checkpoint

data Codec a = Codec
    { _codecEncode :: !(a -> B.ByteString)
        -- ^ encode a value.
    , _codecDecode :: !(forall m. MonadThrow m => B.ByteString -> m a)
        -- ^ decode a value. Throws an exception of decoding fails.
    }

data RocksDB = RocksDB
  { _rocksDbHandle :: !(Ptr RocksDB)
  , _rocksDbNamespace :: !B.ByteString
  }

type ErrPtr = Ptr CString

intToCInt :: Int -> CInt
intToCInt = fromIntegral

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral

boolToNum :: Num b => Bool -> b
boolToNum True  = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)

cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

checked :: HasCallStack => String -> (ErrPtr -> IO a) -> IO a
checked whatWasIDoing act = alloca $ \errPtr -> do
    poke errPtr (nullPtr :: CString)
    r <- act errPtr
    err <- peek errPtr
    unless (err == nullPtr) $ do
        errStr <- B.packCString err
        let msg = unwords ["Data.CAS.RocksDB.checked: error while", whatWasIDoing <> ":", B8.unpack errStr]
        c_rocksdb_free err
        error msg
    return r

-- | Marshal a 'FilePath' (Haskell string) into a `NUL` terminated C string using
-- temporary storage.
-- On Linux, UTF-8 is almost always the encoding used.
-- When on Windows, UTF-8 can also be used, although the default for those devices is
-- UTF-16. For a more detailed explanation, please refer to
-- https://msdn.microsoft.com/en-us/library/windows/desktop/dd374081(v=vs.85).aspx.
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = GHC.withCString GHC.utf8

foreign import ccall safe "rocksdb\\c.h rocksdb_options_create"
    c_rocksdb_options_create :: IO (Ptr Options)

foreign import ccall safe "rocksdb\\c.h rocksdb_options_destroy"
    c_rocksdb_options_destroy :: Ptr Options -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_options_set_create_if_missing"
    c_rocksdb_options_set_create_if_missing :: Ptr Options -> CUChar -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_options_set_error_if_exists"
    c_rocksdb_options_set_error_if_exists :: Ptr Options -> CUChar -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_options_set_paranoid_checks"
    c_rocksdb_options_set_paranoid_checks :: Ptr Options -> CUChar -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_options_set_write_buffer_size"
    c_rocksdb_options_set_write_buffer_size :: Ptr Options -> CSize -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_options_set_max_open_files"
    c_rocksdb_options_set_max_open_files :: Ptr Options -> CInt -> IO ()

foreign import ccall unsafe "rocksdb\\c.h rocksdb_options_set_prefix_extractor"
    rocksdb_options_set_prefix_extractor :: Ptr Options -> Ptr PrefixExtractor -> IO ()

foreign import ccall unsafe "cpp\\chainweb-rocksdb.h rocksdb_options_table_prefix_extractor"
    rocksdb_options_table_prefix_extractor :: Ptr PrefixExtractor

-- | Options when opening a database
data Options = Options
    { createIfMissing :: !Bool
      -- ^ If true, the database will be created if it is missing.
      --
    , maxOpenFiles    :: !Int
      -- ^ Number of open files that can be used by the DB.
      --
      -- You may need to increase this if your database has a large working set
      -- (budget one open file per 2MB of working set).
      --
    , paranoidChecks  :: !Bool
      -- ^ If true, the implementation will do aggressive checking of the data
      -- it is processing and will stop early if it detects any errors.
      --
      -- This may have unforeseen ramifications: for example, a corruption of
      -- one DB entry may cause a large number of entries to become unreadable
      -- or for the entire DB to become unopenable.
      --
    , writeBufferSize :: !Int
      -- ^ Amount of data to build up in memory (backed by an unsorted log on
      -- disk) before converting to a sorted on-disk file.
      --
      -- Larger values increase performance, especially during bulk loads. Up to
      -- to write buffers may be held in memory at the same time, so you may
      -- with to adjust this parameter to control memory usage. Also, a larger
      -- write buffer will result in a longer recovery time the next time the
      -- database is opened.
    }

defaultOptions :: Options
defaultOptions = Options
    { maxOpenFiles = -1
    , writeBufferSize = 64 `shift` 20
    , createIfMissing = True
    , paranoidChecks = False
    }

mkOpts :: Options -> IO (Ptr Options)
mkOpts Options{..} = do
    opts_ptr <- c_rocksdb_options_create

    c_rocksdb_options_set_create_if_missing opts_ptr
        $ boolToNum createIfMissing
    c_rocksdb_options_set_max_open_files opts_ptr
        $ intToCInt maxOpenFiles
    c_rocksdb_options_set_paranoid_checks opts_ptr
        $ boolToNum paranoidChecks
    c_rocksdb_options_set_write_buffer_size opts_ptr
        $ intToCSize writeBufferSize

    return opts_ptr

freeOpts :: Ptr Options -> IO ()
freeOpts opts_ptr =
    c_rocksdb_options_destroy opts_ptr

withOpts :: Options -> (Ptr Options -> IO a) -> IO a
withOpts opts =
    bracket (mkOpts opts) freeOpts

foreign import ccall safe "rocksdb\\c.h rocksdb_open"
    c_rocksdb_open :: Ptr Options -> CString -> ErrPtr -> IO (Ptr RocksDB)

-- | Open a 'RocksDB' instance with the default namespace. If no rocks db exists
-- at the provided directory path, a new database is created.
--
openRocksDB :: FilePath -> Ptr Options -> IO RocksDB
openRocksDB path opts_ptr = do
    GHC.setFileSystemEncoding GHC.utf8
    createDirectoryIfMissing True path
    dbPtr <- withFilePath path $ \path_ptr ->
        checked "opening"
        $ c_rocksdb_open opts_ptr path_ptr
    return $ RocksDB dbPtr ""

foreign import ccall safe "rocksdb\\c.h rocksdb_close"
    c_rocksdb_close :: Ptr RocksDB -> IO ()

closeRocksDB :: RocksDB -> IO ()
closeRocksDB (RocksDB ptr _) = c_rocksdb_close ptr

withRocksDB :: FilePath -> Options -> (RocksDB -> IO a) -> IO a
withRocksDB path opts act =
    withOpts opts $ \opts_ptr ->
        bracket (openRocksDB path opts_ptr) closeRocksDB act

withTempRocksDB :: String -> (RocksDB -> IO a) -> IO a
withTempRocksDB template act = withSystemTempDirectory template $ \dir ->
    withRocksDB dir defaultOptions act

foreign import ccall safe "rocksdb\\c.h rocksdb_destroy_db"
    c_rocksdb_destroy_db :: Ptr Options -> CString -> ErrPtr -> IO ()

destroyRocksDB :: FilePath -> Ptr Options -> IO ()
destroyRocksDB path opts_ptr = do
    withFilePath path $ \path_ptr ->
        checked "destroying" $ c_rocksdb_destroy_db opts_ptr path_ptr

resetOpenRocksDB :: FilePath -> Ptr Options -> IO RocksDB
resetOpenRocksDB path opts = do
    destroyRocksDB path opts
    openRocksDB path opts

foreign import ccall safe "rocksdb\\c.h rocksdb_readoptions_create"
    c_rocksdb_readoptions_create :: IO (Ptr ReadOptions)

foreign import ccall safe "rocksdb\\c.h rocksdb_readoptions_destroy"
    c_rocksdb_readoptions_destroy :: Ptr ReadOptions -> IO ()

createReadOptions :: Initializer ReadOptions -> IO (ForeignPtr ReadOptions)
createReadOptions r =
    allocateInitializer c_rocksdb_readoptions_create c_rocksdb_readoptions_destroy r

withReadOptions :: Initializer ReadOptions -> (Ptr ReadOptions -> IO a) -> IO a
withReadOptions =
    withInitializer c_rocksdb_readoptions_create c_rocksdb_readoptions_destroy

foreign import ccall safe "rocksdb\\c.h rocksdb_readoptions_set_verify_checksums"
    c_rocksdb_readoptions_set_verify_checksums :: Ptr ReadOptions -> CUChar -> IO ()

-- | If true, all data read from underlying storage will be verified
-- against corresponding checksuyms.
setVerifyChecksums :: Bool -> Initializer ReadOptions
setVerifyChecksums b = simpleInitializer
    (flip c_rocksdb_readoptions_set_verify_checksums (boolToNum b))

foreign import ccall safe "rocksdb\\c.h rocksdb_readoptions_set_fill_cache"
    c_rocksdb_readoptions_set_fill_cache :: Ptr ReadOptions -> CUChar -> IO ()

-- -- | Should the data read for this iteration be cached in memory? Callers
-- -- may wish to set this field to false for bulk scans.
setFillCache :: Bool -> Initializer ReadOptions
setFillCache b = simpleInitializer
    (flip c_rocksdb_readoptions_set_fill_cache (boolToNum b))

bsToCStringLen :: ByteString -> IO CStringLen
bsToCStringLen bs = do
    let len = B.length bs
    cs :: CString <- mallocArray len
    copyByteArrayToPtr bs cs
    return (cs, len)

foreign import ccall unsafe "rocksdb\\c.h rocksdb_readoptions_set_iterate_upper_bound"
    rocksdb_readoptions_set_iterate_upper_bound :: Ptr ReadOptions -> CString -> CSize -> IO ()

setUpperBound :: ByteString -> Initializer ReadOptions
setUpperBound upper = Initializer
    (bsToCStringLen upper)
    (\(upperPtr, upperLen) p ->
        rocksdb_readoptions_set_iterate_upper_bound p upperPtr (fromIntegral upperLen))
    (\(upperPtr, _) -> free upperPtr)

foreign import ccall unsafe "rocksdb\\c.h rocksdb_readoptions_set_iterate_lower_bound"
    rocksdb_readoptions_set_iterate_lower_bound :: Ptr ReadOptions -> CString -> CSize -> IO ()

setLowerBound :: ByteString -> Initializer ReadOptions
setLowerBound lower = Initializer
    (bsToCStringLen lower)
    (\(lowerPtr, lowerLen) p ->
        rocksdb_readoptions_set_iterate_lower_bound p lowerPtr (fromIntegral lowerLen))
    (\(lowerPtr, _) -> free lowerPtr)

foreign import ccall unsafe "cpp\\chainweb-rocksdb.h rocksdb_readoptions_set_auto_prefix_mode"
    rocksdb_readoptions_set_auto_prefix_mode :: Ptr ReadOptions -> CBool -> IO ()

setAutoPrefixMode :: Bool -> Initializer ReadOptions
setAutoPrefixMode m = simpleInitializer
    (flip rocksdb_readoptions_set_auto_prefix_mode (boolToNum m))

-- R.Options
-- R.ReadOptions
-- R.WriteOptions

-- data RocksDBException

-- checkpointRocksDb
-- deleteRangeRocksDb
-- compactRangeRocksDb


data RocksDBTable k v = RocksDBTable
    { _rocksDBTableValueCodec :: !(Codec v)
    , _rocksDBTableKeyCodec :: !(Codec k)
    , _rocksDBTableNamespace :: !B.ByteString
    , _rocksDBTableDB :: !RocksDB
    }

data RocksDBIterator k v = RocksDBIterator !(RocksDBTable k v) !(Ptr (RocksDBIterator k v))

foreign import ccall safe "rocksdb\\c.h rocksdb_put"
    c_rocksdb_put
        :: Ptr RocksDB
        -> Ptr WriteOptions
        -> CString -> CSize
        -> CString -> CSize
        -> ErrPtr
        -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_delete"
    c_rocksdb_delete
        :: Ptr RocksDB
        -> Ptr WriteOptions
        -> CString -> CSize
        -> ErrPtr
        -> IO ()

foreign import ccall unsafe "cpp\\chainweb-rocksdb.h rocksdb_delete_range"
    c_rocksdb_delete_range
        :: Ptr RocksDB
        -> Ptr WriteOptions
        -> CString {- min key -}
        -> CSize {- min key length -}
        -> CString {- max key length -}
        -> CSize {- max key length -}
        -> Ptr CString {- output: errptr -}
        -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_write"
    c_rocksdb_write
        :: Ptr RocksDB
        -> Ptr WriteOptions
        -> Ptr WriteBatch
        -> ErrPtr
        -> IO ()

-- | Returns NULL if not found. A malloc()ed array otherwise. Stores the length
-- of the array in *vallen.
foreign import ccall safe "rocksdb\\c.h rocksdb_get"
    c_rocksdb_get
        :: Ptr RocksDB
        -> Ptr ReadOptions
        -> CString -> CSize
        -> Ptr CSize        -- ^ value length
        -> ErrPtr
        -> IO CString

-- // if values_list[i] == NULL and errs[i] == NULL,
-- // then we got status.IsNotFound(), which we will not return.
-- // all errors except status status.ok() and status.IsNotFound() are returned.
-- //
-- // errs, values_list and values_list_sizes must be num_keys in length,
-- // allocated by the caller.
-- // errs is a list of strings as opposed to the conventional one error,
-- // where errs[i] is the status for retrieval of keys_list[i].
-- // each non-NULL errs entry is a malloc()ed, null terminated string.
-- // each non-NULL values_list entry is a malloc()ed array, with
-- // the length for each stored in values_list_sizes[i].
-- extern ROCKSDB_LIBRARY_API void rocksdb_multi_get(
--     rocksdb_t* db, const rocksdb_readoptions_t* options, size_t num_keys,
--     const char* const* keys_list, const size_t* keys_list_sizes,
--     char** values_list, size_t* values_list_sizes, char** errs);
--
foreign import ccall unsafe "rocksdb\\c.h rocksdb_multi_get"
    rocksdb_multi_get
        :: Ptr RocksDB
        -> Ptr ReadOptions
        -> CSize
            -- ^ num_key
        -> Ptr (Ptr CChar)
            -- ^ keys_list
        -> Ptr CSize
            -- ^ keys_list_sizes
        -> Ptr (Ptr CChar)
            -- ^ values_list
        -> Ptr CSize
            -- ^ values_list_sizes
        -> Ptr CString
            -- ^ errs
        -> IO ()

-- | Returns NULL if property name is unknown. Else returns a pointer to a
-- malloc()-ed null-terminated value.
foreign import ccall safe "rocksdb\\c.h rocksdb_property_value"
    c_rocksdb_property_value :: Ptr RocksDB

foreign import ccall safe "rocksdb\\c.h rocksdb_approximate_sizes"
    c_rocksdb_approximate_sizes
        :: Ptr RocksDB
        -> CInt                     -- ^ num ranges
        -> Ptr CString -> Ptr CSize -- ^ range start keys (array)
        -> Ptr CString -> Ptr CSize -- ^ range limit keys (array)
        -> Ptr Word64               -- ^ array of approx. sizes of ranges
        -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_repair_db"
    c_rocksdb_repair_db :: Ptr Options -> CString -> ErrPtr -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_compact_range"
    rocksdb_compact_range
        :: Ptr RocksDB
        -> CString {- min key -}
        -> CSize {- min key length -}
        -> CString {- max key -}
        -> CSize {- max key length -}
        -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_create_iterator"
    c_rocksdb_create_iterator :: Ptr RocksDB -> Ptr ReadOptions -> IO (Ptr (RocksDBIterator k v))

foreign import ccall unsafe "rocksdb\\c.h rocksdb_iter_destroy"
    c_rocksdb_iter_destroy :: Ptr (RocksDBIterator k v) -> IO ()

foreign import ccall unsafe "rocksdb\\c.h rocksdb_iter_valid"
    c_rocksdb_iter_valid :: Ptr (RocksDBIterator k v) -> IO CUChar

foreign import ccall safe "rocksdb\\c.h rocksdb_iter_seek_to_first"
    c_rocksdb_iter_seek_to_first :: Ptr (RocksDBIterator k v) -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_iter_seek_to_last"
    c_rocksdb_iter_seek_to_last :: Ptr (RocksDBIterator k v ) -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_iter_seek"
    c_rocksdb_iter_seek :: Ptr (RocksDBIterator k v) -> CString -> CSize -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_iter_next"
    c_rocksdb_iter_next :: Ptr (RocksDBIterator k v) -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_iter_prev"
    c_rocksdb_iter_prev :: Ptr (RocksDBIterator k v) -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_iter_key"
    c_rocksdb_iter_key :: Ptr (RocksDBIterator k v) -> Ptr CSize -> IO CString

foreign import ccall safe "rocksdb\\c.h rocksdb_iter_value"
    c_rocksdb_iter_value :: Ptr (RocksDBIterator k v) -> Ptr CSize -> IO CString

foreign import ccall safe "rocksdb\\c.h rocksdb_iter_get_error"
    c_rocksdb_iter_get_error :: Ptr (RocksDBIterator k v) -> ErrPtr -> IO ()


--
-- Write batch
--

foreign import ccall safe "rocksdb\\c.h rocksdb_writebatch_create"
    c_rocksdb_writebatch_create :: IO (Ptr WriteBatch)

foreign import ccall safe "rocksdb\\c.h rocksdb_writebatch_destroy"
    c_rocksdb_writebatch_destroy :: Ptr WriteBatch -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_writebatch_clear"
    c_rocksdb_writebatch_clear :: Ptr WriteBatch -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_writebatch_put"
    c_rocksdb_writebatch_put :: Ptr WriteBatch
                           -> CString -> CSize
                           -> CString -> CSize
                           -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_writebatch_delete"
    c_rocksdb_writebatch_delete :: Ptr WriteBatch -> CString -> CSize -> IO ()

--
-- Read options
--

--
-- Write options
--

foreign import ccall safe "rocksdb\\c.h rocksdb_writeoptions_create"
    c_rocksdb_writeoptions_create :: IO (Ptr WriteOptions)

foreign import ccall safe "rocksdb\\c.h rocksdb_writeoptions_destroy"
    c_rocksdb_writeoptions_destroy :: Ptr WriteOptions -> IO ()

foreign import ccall safe "rocksdb\\c.h rocksdb_writeoptions_set_sync"
    c_rocksdb_writeoptions_set_sync :: Ptr WriteOptions -> CUChar -> IO ()

foreign import ccall unsafe "rocksdb\\c.h rocksdb_free"
    c_rocksdb_free :: CString -> IO ()

----------------------------------------------------------------------------
-- Checkpoints
----------------------------------------------------------------------------

foreign import ccall unsafe "rocksdb\\c.h rocksdb_checkpoint_object_create"
    rocksdb_checkpoint_object_create :: Ptr RocksDB -> Ptr CString -> IO (Ptr Checkpoint)

foreign import ccall unsafe "rocksdb\\c.h rocksdb_checkpoint_create"
    rocksdb_checkpoint_create :: Ptr Checkpoint -> CString -> CULong -> Ptr CString -> IO ()

foreign import ccall unsafe "rocksdb\\c.h rocksdb_checkpoint_object_destroy"
    rocksdb_checkpoint_object_destroy :: Ptr Checkpoint -> IO ()
