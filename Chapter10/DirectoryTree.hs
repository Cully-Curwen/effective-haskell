{-# LANGUAGE TypeApplications #-}

module DirectoryTree where

import Control.Exception (IOException, handle)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import qualified Data.Set as Set (empty, insert, member)
import Text.Printf (printf)

dropSuffix :: String -> String -> String
dropSuffix suffix s
  | suffix `isSuffixOf` s =
  take (length s - length suffix) s
  | otherwise = s

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile
  | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
    (True, False) -> FileTypeDirectory
    (False, True) -> FileTypeRegularFile
    _otherwise    -> FileTypeOther

naiveTraversal :: FilePath -> (FilePath -> a) -> IO [a]
naiveTraversal rootPath action = do
  classification <- classifyFile rootPath
  case classification of
    FileTypeOther ->
      pure []
    FileTypeRegularFile ->
      pure $ [action rootPath]
    FileTypeDirectory -> do
      contents <- map (fixPath rootPath) <$> listDirectory rootPath
      results <- concat <$> getPaths contents
      pure results
    where
      fixPath parent fname = parent <> "/" <> fname
      getPaths = mapM (\path -> naiveTraversal path action)

traverseDirectory :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory rootPath action = do
  seenRef <- newIORef Set.empty
  resultRef <- newIORef []
  let
    haveSeenDirectory canonicalPath =
      Set.member canonicalPath <$> readIORef seenRef
    addDirectoryToSeen canonicalPath =
      modifyIORef seenRef $ Set.insert canonicalPath
    traverseSubdirectory subdirPath = do
      contents <- listDirectory subdirPath
      for_ contents $ \file' ->
        handle @IOException (\_ -> pure ()) $ do
        let file = subdirPath <> "/" <> file'
        canonicalPath <- canonicalizePath file
        classification <- classifyFile canonicalPath
        case classification of
          FileTypeOther -> pure ()
          FileTypeRegularFile ->
            modifyIORef resultRef (\results -> action file : results)
          FileTypeDirectory -> do
            alreadyProcessed <- haveSeenDirectory file
            when (not alreadyProcessed) $ do
              addDirectoryToSeen file
              traverseSubdirectory file
  traverseSubdirectory (dropSuffix "/" rootPath)
  readIORef resultRef

