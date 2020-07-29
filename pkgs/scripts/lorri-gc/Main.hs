{-# language ImportQualifiedPost #-}
{-# language TypeApplications #-}

-- | Script for finding old lorri files and optionally removing them
module Main (main) where

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.List (dropWhileEnd, isInfixOf, find)
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import System.Environment (getArgs, getEnv)

import System.Directory (listDirectory, removePathForcibly)
import System.FilePath ((</>), dropFileName)

import Crypto.Hash (hash, MD5)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC

import Data.Set (Set)
import Data.Set qualified as S

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

direnvAllowedDirs :: FilePath -> IO (Set FilePath)
direnvAllowedDirs home = do
  let direnvAllowDir = home </> ".local/share/direnv/allow"
  direnvAllowFiles <- map (direnvAllowDir </>) <$> listDirectory direnvAllowDir
  -- Each file contains the path to an .envrc
  S.fromList . map (dropFileName . trim) <$> traverse readFile direnvAllowFiles

lorriGcRootHashes :: FilePath -> IO (Set String)
lorriGcRootHashes home = S.fromList <$> listDirectory lorriGcRootDir
  where lorriGcRootDir = home </> ".cache/lorri/gc_roots"

-- | Given a path to a project directory, return the hash lorri uses for it
lorriGcHash :: FilePath -> String
lorriGcHash project = show $ hash @ByteString @MD5 $ BC.pack $ project </> "shell.nix"

lorriCasFiles :: FilePath -> IO [FilePath]
lorriCasFiles home = fmap (lorriCasDir </>) <$> listDirectory lorriCasDir
  where lorriCasDir = home </> ".cache/lorri/cas"

findExtraFiles :: IO ([FilePath], [FilePath])
findExtraFiles = do
  home <- getEnv "HOME"
  expectedHashes <- S.map lorriGcHash <$> direnvAllowedDirs home
  actualHashes <- lorriGcRootHashes home
  let extraHashes = S.toList (actualHashes S.\\ expectedHashes)
  let extraGcRoots = fmap (\h -> home </> ".cache/lorri/gc_roots" </> h) extraHashes
  casFiles <- lorriCasFiles home
  extraCasFiles <- fmap catMaybes $ for casFiles $ \path -> do
    firstFewLines <- concat . take 3 . lines <$> readFile path
    pure $ path <$ find (`isInfixOf` firstFewLines) extraHashes
  pure (extraGcRoots, extraCasFiles)

main :: IO ()
main = do
  let casWarning extraCasFiles = unless (null extraCasFiles) $ do
        putStrLn "WARNING: this cannot detect all stale CAS files, only those that were made stale by this GC"
  args <- getArgs
  case args of
    ["-p"] -> do
      (extraGcRoots, extraCasFiles) <- findExtraFiles
      putStrLn "Lorri GC roots to remove:"
      traverse_ (putStrLn . ("  " ++ )) extraGcRoots
      putStrLn "\nLorri CAS files to remove:"
      traverse_ (putStrLn . ("  " ++ )) extraCasFiles
      casWarning extraCasFiles
    ["-r"] -> do
      (extraGcRoots, extraCasFiles) <- findExtraFiles
      let printAndRemove path = putStrLn path *> removePathForcibly path
      traverse_ printAndRemove extraGcRoots
      traverse_ printAndRemove extraCasFiles
      casWarning extraCasFiles
    _ -> putStrLn "Usage: lorri-gc [-p|-r]"

