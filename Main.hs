module Main where

import Crypto.Hash.SHA1 (hashlazy)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap as HM
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (die)

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

findDuplicates :: [FilePath] -> IO ()
findDuplicates paths = do
  entrys <- mapM reqList paths
  -- mapM_ putStrLn entrys
  hashes <- mapM (\p -> LBS.readFile p >>= (\bs -> return (hashlazy bs, p))) $ merge entrys
  -- mapM_ (\(h, p) -> putStr (Char8.unpack $ B.toLazyByteString $ B.byteStringHex h) >> putStr " : " >> putStrLn p) hashes
  let doups = filterDuplicates hashes
  mapM_ (\(_, ps) -> mapM_ (\p -> putStr p >> putStr "\t") ps >> putStr "\n") $ HM.toList doups

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
