//  Copyright (c) 2011-present, Facebook, Inc.  All rights reserved.
//  This source code is licensed under both the GPLv2 (found in the
//  COPYING file in the root directory) and Apache 2.0 License
//  (found in the LICENSE.Apache file in the root directory).
//
// Copyright (c) 2011 The LevelDB Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file. See the AUTHORS file for names of contributors.

// The code not taken from the LevelDB and RocksDB authors is 
// Copyright (c) 2022 Kadena.

#include "rocksdb/c.h"

// copied from rocksdb/c.cc
#include <stdlib.h>
#include "rocksdb/cache.h"
#include "rocksdb/compaction_filter.h"
#include "rocksdb/comparator.h"
#include "rocksdb/convenience.h"
#include "rocksdb/db.h"
#include "rocksdb/env.h"
#include "rocksdb/filter_policy.h"
#include "rocksdb/iterator.h"
#include "rocksdb/memtablerep.h"
#include "rocksdb/merge_operator.h"
#include "rocksdb/options.h"
#include "rocksdb/rate_limiter.h"
#include "rocksdb/slice_transform.h"
#include "rocksdb/statistics.h"
#include "rocksdb/status.h"
#include "rocksdb/table.h"
#include "rocksdb/universal_compaction.h"
#include "rocksdb/utilities/backupable_db.h"
#include "rocksdb/utilities/checkpoint.h"
#include "rocksdb/utilities/db_ttl.h"
#include "rocksdb/utilities/memory_util.h"
#include "rocksdb/utilities/optimistic_transaction_db.h"
#include "rocksdb/utilities/transaction.h"
#include "rocksdb/utilities/transaction_db.h"
#include "rocksdb/utilities/write_batch_with_index.h"
#include "rocksdb/write_batch.h"
#include "rocksdb/perf_context.h"

#include <vector>
#include <unordered_set>
#include <map>

using rocksdb::BytewiseComparator;
using rocksdb::Cache;
using rocksdb::ColumnFamilyDescriptor;
using rocksdb::ColumnFamilyHandle;
using rocksdb::ColumnFamilyOptions;
using rocksdb::CompactionFilter;
using rocksdb::CompactionFilterFactory;
using rocksdb::CompactionFilterContext;
using rocksdb::CompactionOptionsFIFO;
using rocksdb::Comparator;
using rocksdb::CompressionType;
using rocksdb::WALRecoveryMode;
using rocksdb::DB;
using rocksdb::DBOptions;
using rocksdb::DbPath;
using rocksdb::Env;
using rocksdb::EnvOptions;
using rocksdb::InfoLogLevel;
using rocksdb::FileLock;
using rocksdb::FilterPolicy;
using rocksdb::FlushOptions;
using rocksdb::IngestExternalFileOptions;
using rocksdb::Iterator;
using rocksdb::Logger;
using rocksdb::MergeOperator;
using rocksdb::NewBloomFilterPolicy;
using rocksdb::NewLRUCache;
using rocksdb::Options;
using rocksdb::BlockBasedTableOptions;
using rocksdb::CuckooTableOptions;
using rocksdb::RandomAccessFile;
using rocksdb::Range;
using rocksdb::ReadOptions;
using rocksdb::SequentialFile;
using rocksdb::Slice;
using rocksdb::SliceParts;
using rocksdb::SliceTransform;
using rocksdb::Snapshot;
using rocksdb::SstFileWriter;
using rocksdb::Status;
using rocksdb::WritableFile;
using rocksdb::WriteBatch;
using rocksdb::WriteBatchWithIndex;
using rocksdb::WriteOptions;
using rocksdb::LiveFileMetaData;
using rocksdb::BackupEngine;
using rocksdb::BackupableDBOptions;
using rocksdb::BackupInfo;
using rocksdb::BackupID;
using rocksdb::RestoreOptions;
using rocksdb::CompactRangeOptions;
using rocksdb::BottommostLevelCompaction;
using rocksdb::RateLimiter;
using rocksdb::NewGenericRateLimiter;
using rocksdb::PinnableSlice;
using rocksdb::TransactionDBOptions;
using rocksdb::TransactionDB;
using rocksdb::TransactionOptions;
using rocksdb::OptimisticTransactionDB;
using rocksdb::OptimisticTransactionOptions;
using rocksdb::Transaction;
using rocksdb::Checkpoint;
using rocksdb::TransactionLogIterator;
using rocksdb::BatchResult;
using rocksdb::PerfLevel;
using rocksdb::PerfContext;
using rocksdb::MemoryUtil;

using std::shared_ptr;
using std::vector;
using std::unordered_set;
using std::map;

extern "C" {

struct rocksdb_t                 { DB*               rep; };
struct rocksdb_backup_engine_t   { BackupEngine*     rep; };
struct rocksdb_backup_engine_info_t { std::vector<BackupInfo> rep; };
struct rocksdb_restore_options_t { RestoreOptions rep; };
struct rocksdb_iterator_t        { Iterator*         rep; };
struct rocksdb_writebatch_t      { WriteBatch        rep; };
struct rocksdb_writebatch_wi_t   { WriteBatchWithIndex* rep; };
struct rocksdb_snapshot_t        { const Snapshot*   rep; };
struct rocksdb_flushoptions_t    { FlushOptions      rep; };
struct rocksdb_fifo_compaction_options_t { CompactionOptionsFIFO rep; };
struct rocksdb_readoptions_t {
   ReadOptions rep;
   // stack variables to set pointers to in ReadOptions
   Slice upper_bound;
   Slice lower_bound;
};
struct rocksdb_writeoptions_t    { WriteOptions      rep; };
struct rocksdb_options_t         { Options           rep; };
struct rocksdb_compactoptions_t {
  CompactRangeOptions rep;
};
struct rocksdb_block_based_table_options_t  { BlockBasedTableOptions rep; };
struct rocksdb_cuckoo_table_options_t  { CuckooTableOptions rep; };
struct rocksdb_seqfile_t         { SequentialFile*   rep; };
struct rocksdb_randomfile_t      { RandomAccessFile* rep; };
struct rocksdb_writablefile_t    { WritableFile*     rep; };
struct rocksdb_wal_iterator_t { TransactionLogIterator* rep; };
struct rocksdb_wal_readoptions_t { TransactionLogIterator::ReadOptions rep; };
struct rocksdb_filelock_t        { FileLock*         rep; };
struct rocksdb_logger_t          { shared_ptr<Logger>  rep; };
struct rocksdb_cache_t           { shared_ptr<Cache>   rep; };
struct rocksdb_livefiles_t       { std::vector<LiveFileMetaData> rep; };
struct rocksdb_column_family_handle_t  { ColumnFamilyHandle* rep; };
struct rocksdb_envoptions_t      { EnvOptions        rep; };
struct rocksdb_ingestexternalfileoptions_t  { IngestExternalFileOptions rep; };
struct rocksdb_sstfilewriter_t   { SstFileWriter*    rep; };
struct rocksdb_ratelimiter_t     { shared_ptr<RateLimiter>      rep; };
struct rocksdb_perfcontext_t     { PerfContext*      rep; };
struct rocksdb_pinnableslice_t {
  PinnableSlice rep;
};
struct rocksdb_transactiondb_options_t {
  TransactionDBOptions rep;
};
struct rocksdb_transactiondb_t {
  TransactionDB* rep;
};
struct rocksdb_transaction_options_t {
  TransactionOptions rep;
};
struct rocksdb_transaction_t {
  Transaction* rep;
};
struct rocksdb_checkpoint_t {
  Checkpoint* rep;
};
struct rocksdb_optimistictransactiondb_t {
  OptimisticTransactionDB* rep;
};
struct rocksdb_optimistictransaction_options_t {
  OptimisticTransactionOptions rep;
};

static bool SaveError(char** errptr, const Status& s) {
  assert(errptr != nullptr);
  if (s.ok()) {
    return false;
  } else if (*errptr == nullptr) {
    *errptr = strdup(s.ToString().c_str());
  } else {
    // TODO(sanjay): Merge with existing error?
    // This is a bug if *errptr is not created by malloc()
    free(*errptr);
    *errptr = strdup(s.ToString().c_str());
  }
  return true;
}
// end of code copied from rocksdb/c.cc

void rocksdb_delete_range(rocksdb_t* db,
                          const rocksdb_writeoptions_t* options,
                          const char* start_key, size_t start_key_len,
                          const char* end_key, size_t end_key_len,
                          char** errptr) {
      SaveError(errptr, db->rep->DeleteRange(options->rep, nullptr,
                                         Slice(start_key, start_key_len),
                                         Slice(end_key, end_key_len)));
}

}
