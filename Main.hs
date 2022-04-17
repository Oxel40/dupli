module Main where

import Crypto.Hash.SHA1 (hashlazy)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap as HM
import System.CPUTime (getCPUTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs, getProgName)
import System.IO
import System.Exit (die)

nWorkers :: Int
nWorkers = 4

debug :: Bool
debug = True

type DupMap = HM.Map ByteString [FilePath]

merge :: [[a]] -> [a]
merge = merge' []

merge' :: [a] -> [[a]] -> [a]
merge' [] [] = []
merge' [] (b:bs) = merge' b bs
merge' (x:xs) bs = x : merge' xs bs

main :: IO ()
main = do
  -- putStrLn "Hello, Haskell!"
  args <- getArgs
  name <- getProgName
  if not (null args)
  then findDuplicates args
  else die $ "Not enougth input arguments.\n\
             \Usage: " ++ name ++ " path [paths]\n\
             \Matching files are seperated by a tab character \"\\t\" \
             \and matches are seperated by a newline \"\\n\".\n"

forkThread :: IO () -> IO (MVar ())
forkThread proc = do
  handle <- newEmptyMVar
  _ <- forkFinally proc (\e -> do
    putMVar handle ()
    case e of
      Left ex -> throwIO ex
      Right _ -> return ()
    )
  return handle

forkThreads :: Int -> IO () -> IO [MVar ()]
forkThreads n proc = mapM (\_ -> forkThread proc) [1..n] 

awaitThreads :: [MVar ()] -> IO ()
awaitThreads = mapM_ takeMVar

hashWorker :: TChan (Maybe FilePath) -> TChan (ByteString, FilePath) -> IO ()
hashWorker pathChan hashChan = do
  tId <- myThreadId
  when debug $ putStrLn $ show tId ++ ": Started"
  hw pathChan hashChan

hw :: TChan (Maybe FilePath) -> TChan (ByteString, FilePath) -> IO ()
hw pathChan hashChan = do
  -- threadDelay 1000000 -- 1 sec
  tId <- myThreadId
  mby <- atomically $ readTChan pathChan

  case mby of
    Nothing -> when debug $ putStrLn $ show tId ++ ": Nothing in channel"
    Just path -> do
      -- when debug $ putStrLn $ show tId ++ ": Just in channel"
      withFile path ReadMode $ \file -> do
        -- fileContent <- LBS.readFile path 
        fileContent <- LBS.hGetContents file
        let fileHash = hashlazy fileContent 
        let out = fileHash `seq` (fileHash, path)
        atomically $ writeTChan hashChan $! out
      -- when debug $ putStrLn $ show tId ++ ": Wrote to channel"
      hw pathChan hashChan

  -- threadDelay 1000000 -- 1 sec
      -- hashWorker pathChan hashChan
-- hashes <- mapM (\p -> LBS.readFile p >>= (\bs -> return (hashlazy bs, p))) $ merge entrys

findDuplicates :: [FilePath] -> IO ()
findDuplicates paths = do
  time0 <- getCPUTime
  entrys <- mapM reqList paths
  ----------
  time1 <- getCPUTime

  pathChan <- newTChanIO
  hashChan <- newTChanIO

  let worker = hashWorker pathChan hashChan

  -- start threads
  threads <- forkThreads nWorkers worker
  -- feed workers
  print $ length $ merge entrys
  -- let allPaths = take 1014 $ merge entrys
  let allPaths = merge entrys
  threadDelay 3000000
  mapM_ (atomically . writeTChan pathChan . Just) allPaths
  mapM_ (atomically . writeTChan pathChan . const Nothing) [1..(nWorkers*2)]
  threadDelay 3000000
  -- await threads
  when debug $ putStrLn "Awaiting threads..."
  awaitThreads threads
  when debug $ putStrLn "Awaiting threads... Done"
  -- hashes <- getChanContents hashChan
  when debug $ putStrLn "Reading from chan..."
  -- _ <- atomically $ readTChan hashChan 
  {-
  i <- newMVar 1 :: IO (MVar Int)
  _ <- forever $ do
    im <- takeMVar i
    mb <- atomically $ tryReadTChan hashChan
    case mb of
      Nothing -> do
        when debug $ putStrLn $ show im ++ " Nothing in channel"
        _ <- atomically $ readTChan hashChan
        return ()
      Just _ -> return ()
    -- when debug $ putStrLn $ show im ++ " Single read from chan done"
    putMVar i (im+1)
  -}

  hashes <- mapM (\_ -> atomically $ readTChan hashChan) allPaths
  when debug $ putStrLn "Reading from chan... Done"

  ----------
  time2 <- getCPUTime
  let doups = filterDuplicates hashes
  ----------
  time3 <- getCPUTime
  mapM_ (\(_, ps) -> mapM_ (\p -> putStr p >> putStr "\t") ps >> putStr "\n") $ HM.toList doups

  when debug $ print $ length hashes

  putStr "Finding entries: "
  print $ (time1 - time0)*100 `div` (time3 - time0)
  putStr "Reading files and generating hashes: "
  print $ (time2 - time1)*100 `div` (time3 - time0)
  putStr "Filtering doup hashes: "
  print $ (time3 - time2)*100 `div` (time3 - time0)

filterDuplicates :: [(ByteString, FilePath)] -> DupMap
filterDuplicates = fd HM.empty

fd :: DupMap -> [(ByteString, FilePath)] -> DupMap
fd hMap [] = HM.filter (\ps -> length ps > 1) hMap
fd hMap ((bs, p) : rest)
  | HM.member bs hMap = fd hMap' rest
  | otherwise = fd hMap'' rest
  where
    hMap' = HM.adjust (p:) bs hMap 
    hMap'' = HM.insert bs [p] hMap

reqList :: FilePath -> IO [FilePath]
reqList path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir
    then do
      subPaths <- listDirectory path
      files <- mapM (\p -> reqList (path ++ ('/' : p))) subPaths
      return $ concat files
    else
      if isFile
        then return [path]
        else die $ "Error: " ++ path ++ " is not a file or directory."
