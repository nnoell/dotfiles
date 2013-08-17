{-# LINE 1 "StatFS.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "StatFS.hsc" #-}
-- |
-- Module      :  StatFS
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  A binding to C's statvfs(2)
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}


module StatFS ( FileSystemStats(..), getFileSystemStats ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)


{-# LINE 29 "StatFS.hsc" #-}


{-# LINE 34 "StatFS.hsc" #-}

{-# LINE 35 "StatFS.hsc" #-}

{-# LINE 36 "StatFS.hsc" #-}

data FileSystemStats = FileSystemStats {
  fsStatBlockSize :: Integer
  -- ^ Optimal transfer block size.
  , fsStatBlockCount :: Integer
  -- ^ Total data blocks in file system.
  , fsStatByteCount :: Integer
  -- ^ Total bytes in file system.
  , fsStatBytesFree :: Integer
  -- ^ Free bytes in file system.
  , fsStatBytesAvailable :: Integer
  -- ^ Free bytes available to non-superusers.
  , fsStatBytesUsed :: Integer
  -- ^ Bytes used.
  } deriving (Show, Eq)

data CStatfs


{-# LINE 57 "StatFS.hsc" #-}
foreign import ccall unsafe "sys/statvfs.h statvfs"

{-# LINE 59 "StatFS.hsc" #-}
  c_statfs :: CString -> Ptr CStatfs -> IO CInt

toI :: CULong -> Integer
toI = toInteger

getFileSystemStats :: String -> IO (Maybe FileSystemStats)
getFileSystemStats path =
  allocaBytes ((120)) $ \vfs ->
{-# LINE 67 "StatFS.hsc" #-}
  useAsCString (pack path) $ \cpath -> do
    res <- c_statfs cpath vfs
    if res /= 0 then return Nothing
      else do
        bsize <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) vfs
{-# LINE 72 "StatFS.hsc" #-}
        bcount <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) vfs
{-# LINE 73 "StatFS.hsc" #-}
        bfree <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) vfs
{-# LINE 74 "StatFS.hsc" #-}
        bavail <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) vfs
{-# LINE 75 "StatFS.hsc" #-}
        let bpb = toI bsize
        return $ Just FileSystemStats
                       { fsStatBlockSize = bpb
                       , fsStatBlockCount = toI bcount
                       , fsStatByteCount = toI bcount * bpb
                       , fsStatBytesFree = toI bfree * bpb
                       , fsStatBytesAvailable = toI bavail * bpb
                       , fsStatBytesUsed = toI (bcount - bfree) * bpb
                       }
