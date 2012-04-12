{-# LANGUAGE DeriveDataTypeable #-}
-- | This module defines a uniform interface for interacting with
-- compressed archives (zip files and tarballs).  The interface is
-- small:
--
--  1) Read an archive using either the 'readArchive' helper (which
--     reads a file off of disk) or 'decodeArchive' (which works on
--     ByteStrings).
--
--  2) List files in the archive using 'archiveEntries'
--
--  3) Extract files using 'entryContent', which returns a ByteString
--
-- Example:
--
-- > archive <- readArchive "/tmp/gsl-1.15.tar.gz"
-- > let Just configScript = entryContent archive "configure"
module Codec.Archive (
  -- * Types
  ArchiveIndex,
  ArchiveFormat(..),
  ArchiveException(..),
  -- * Constructors
  readArchive,
  decodeArchive,
  -- * Accessors
  archiveEntries,
  entryContent,
  entryContentSuffix
  ) where

import Control.Exception
import Data.ByteString.Lazy ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import Data.Char ( toLower )
import Data.List ( find, isSuffixOf )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Typeable ( Typeable )
import System.FilePath

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Compression.GZip as GZip

data ArchiveFormat = Tar
                   | Zip
                   | TarGz
                   | TarBz2
                     -- Implement this eventually, maybe using
                     -- lzma-enumerator.  Make optional?
                     -- | TarXz

-- | The abstract handle representing a compressed archive
data ArchiveIndex = TarArchive !(Map FilePath Tar.Entry)
                  | ZipArchive !Zip.Archive

-- | The errors that the library can report
data ArchiveException = UnrecognizedFormatError String -- ^ The format of the archive could not be determined (or is unsupported)
                      | TarDecodeError Tar.FormatError -- ^ An error occured while decoding a tar archive
                      | TarEntryIsNotFile String -- ^ The named tar archive entry is not a normal file
                      deriving (Show, Typeable)
instance Exception ArchiveException

-- | Read an archive and guess its format based on its filename.
-- Non-standard extensions (or missing extensions) will fail.
readArchive :: FilePath -> IO ArchiveIndex
readArchive p = do
  let fmt = classifyArchive p
  c <- LBS.readFile p
  return $! decodeArchive fmt c

classifyArchive :: FilePath -> ArchiveFormat
classifyArchive p = case splitExtension (map toLower p) of
  (_, ".zip") -> Zip
  (_, ".tbz2") -> TarBz2
  (_, ".tgz") -> TarGz
  (rest, ".bz2") ->
    case takeExtension rest of
      ".tar" -> TarBz2
      _ -> throw $ UnrecognizedFormatError p
  (rest, ".gz") ->
    case takeExtension rest of
      ".tar" -> TarGz
      _ -> throw $ UnrecognizedFormatError p
  _ -> throw $ UnrecognizedFormatError p

-- | Read an archive from a ByteString, with a given archive format.
decodeArchive :: ArchiveFormat -> ByteString -> ArchiveIndex
decodeArchive Zip content = ZipArchive zarch
  where
    zarch = Zip.toArchive content
decodeArchive TarGz content = decodeArchive Tar (GZip.decompress content)
decodeArchive TarBz2 content = decodeArchive Tar (BZip.decompress content)
decodeArchive Tar content = TarArchive entryMap
  where
    es = Tar.read content
    entryMap = Tar.foldEntries fillMap M.empty (throw . TarDecodeError) es
    fillMap e m = M.insert (Tar.entryPath e) e m

-- | Retrieve the list of all files in the archive
archiveEntries :: ArchiveIndex -> [FilePath]
archiveEntries (TarArchive ix) = M.keys ix
archiveEntries (ZipArchive zarch) = Zip.filesInArchive zarch

-- | Retrieve the contents of the named file from the archive.  If the
-- requested file is not in the archive, this function returns Nothing
entryContent :: ArchiveIndex -> FilePath -> Maybe ByteString
entryContent (TarArchive ix) p = do
  e <- M.lookup p ix
  case Tar.entryContent e of
    Tar.NormalFile bs _ -> return bs
    _ -> throw $ TarEntryIsNotFile p
entryContent (ZipArchive zarch) p = do
  e <- Zip.findEntryByPath p zarch
  return (Zip.fromEntry e)

-- | Get the archive entry (if any) such that the file path of the
-- archive entry is a *suffix* of @p@.  This is useful to ignore some
-- absolute prefix attached to @p@ that would match the path in the
-- archive if it was extracted at the correct location.
entryContentSuffix :: ArchiveIndex -> FilePath -> Maybe ByteString
entryContentSuffix (TarArchive ix) p = do
  (_, e) <- find (matchPrefix p) (M.assocs ix)
  case Tar.entryContent e of
    Tar.NormalFile bs _ -> return bs
    _ -> throw $ TarEntryIsNotFile p
  where
    matchPrefix fp (arcPath, _) = arcPath `isSuffixOf` fp
entryContentSuffix (ZipArchive zarch) p = do
  e <- find (matchPrefix p) (Zip.zEntries zarch)
  return (Zip.fromEntry e)
  where
    matchPrefix fp e = Zip.eRelativePath e `isSuffixOf` fp
