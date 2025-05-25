{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Watcher (runWatcher) where

import Control.Concurrent
  ( MVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , readMVar
  , takeMVar
  , threadDelay, swapMVar
  )
import Control.Monad (forever, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Time
import System.Directory.OsPath (doesFileExist)
import System.Exit qualified as Exit
import System.FSNotify qualified as FSNotify
import System.FilePath qualified as FP
import System.OsPath (OsPath)
import System.OsPath qualified as OSP
import System.Process qualified as Process

exerciseList :: [FilePath]
exerciseList =
  [ "01_hello_world"
  , "02_addition"
  ]

exercisesDirFP :: FilePath
exercisesDirFP = "exercises/"

marker :: ByteString
marker = "-- I AM NOT DONE"

debounceDelay :: Int
debounceDelay = 200 * 1000 -- 200 milliseconds

type DebounceMap = Map.Map FilePath UTCTime

isExerciseIncomplete :: OsPath -> IO Bool
isExerciseIncomplete filePath = do
  exists <- doesFileExist filePath
  if not exists
    then return False
    else do
      content <- BS.readFile =<< OSP.decodeFS filePath
      return (marker `BS.isInfixOf` content)

compileAndRun :: OsPath -> IO ()
compileAndRun exercisePathOSP = do
  putStrLn "----------------------------------------------------"
  putStrLn . ("Attempting to compile: " <>) =<< OSP.decodeFS exercisePathOSP
  let baseName = OSP.takeBaseName exercisePathOSP
  let executableName = [OSP.osp|.|] OSP.</> (baseName <> [OSP.osp|-exercise-exe|])

  exeStr <- OSP.decodeFS executableName
  srcStr <- OSP.decodeFS exercisePathOSP

  (exitCodeCompile, _stdoutCompile, stderrCompile) <-
    Process.readProcessWithExitCode "ghc" ["-o", exeStr, srcStr] ""
  case exitCodeCompile of
    Exit.ExitSuccess -> do
      putStrLn "Compilation successful! 🎉"
      (_exitCodeRun, stdoutRun, stderrRun) <-
        Process.readProcessWithExitCode exeStr [] ""
      putStr stdoutRun
      unless (null stderrRun) $ do
        putStrLn "--- Error Output ---"
        putStrLn stderrRun
      isStillIncomplete <- isExerciseIncomplete exercisePathOSP
      if isStillIncomplete
        then
          putStrLn
            "🚧 '-- I AM NOT DONE' marker still found. Please complete the exercise."
        else do
          exercisePathFP <- OSP.decodeFS exercisePathOSP
          putStrLn $ "✅ Exercise '" ++ exercisePathFP ++ "' completed successfully!"
    _ -> do
      putStrLn "Compilation failed! 🚨"
      putStrLn stderrCompile

runWatcher :: IO ()
runWatcher = do
  putStrLn "🔍 Watching for changes in exercises..."
  for_ exerciseList $ \exFp -> do
    let exFpDir = exercisesDirFP FP.</> exFp
    runExercise exFpDir

runExercise :: FilePath -> IO ()
runExercise exFpDir = do
  stopWatchingMVar :: MVar () <- newEmptyMVar
  buildingMVar :: MVar Bool <- newMVar False
  FSNotify.withManager $ \mgr -> do
    stopManagerAction <-
      FSNotify.watchDir
        mgr
        exFpDir
        (const True)
        (handleEvent stopWatchingMVar)
    takeMVar stopWatchingMVar
    stopManagerAction

handleEvent :: MVar () -> MVar Bool -> FSNotify.Event -> IO ()
handleEvent stopWatchingMvar buildingMVar event = do
  readMVar buildingMVar >>= \case
    False | "Main.hs" <- FSNotify.eventPath event -> do
        _ <- swapMVar buildingMVar True
        trigger path
    _ -> pure ()
  where
    trigger :: FilePath -> IO ()
    trigger path = do
      ospth <- OSP.encodeFS path
      putStrLn ("\n📁 Detected change in: " ++ path)
      isIncomplete <- isExerciseIncomplete ospth
      when isIncomplete $ compileAndRun ospth
