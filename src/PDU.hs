-- Parallelized version of the unix "du" command that shows only the size of current *subdirectories.
-- pdu launches a new thread for each subdirectory, using the neat independence of subfolders.
-- (c) ane <ane@iki.fi>
-- license: BSDv3

module Main where

import System
import System.IO
import System.Directory
import System.FilePath
import Control.Exception
import Control.Monad
import qualified Control.Monad.Parallel as MP
import Data.Maybe
import Data.List
import Data.List.Utils

getSaneDirectoryContents :: FilePath -> IO [FilePath]
getSaneDirectoryContents path = do
  everything <- getDirectoryContents path
  mapM (return . concatPaths path) (filter (`notElem` [".", ".."]) everything)

getTopLevelDirectories :: FilePath -> IO [FilePath]
getTopLevelDirectories path = do
  dotless <- getSaneDirectoryContents path
  filterM doesDirectoryExist dotless

getDirectorySize :: FilePath -> IO Integer
getDirectorySize path = do
  dotless <- getSaneDirectoryContents path
  subd <- filterM doesDirectoryExist dotless
  files <- filterM doesFileExist dotless
  sizes <- mapM getFileSize files
  subDirSizes <- mapM getDirectorySize subd
  return $ sum (map (fromMaybe 0) sizes) + sum subDirSizes

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle t $ bracket (openFile path ReadMode) hClose
  (\h -> do size <- hFileSize h
            return $ Just size)
  where 
    t :: SomeException -> IO (Maybe Integer)
    t _ = return Nothing

concatPaths :: String -> String -> FilePath
concatPaths p1 p2 = joinPath [p1, p2]

getPathSize :: FilePath -> IO [(FilePath, Integer)]
getPathSize path = do
  dirs <- getTopLevelDirectories path
  sizes <- MP.mapM getDirectorySize dirs
  return $ zip dirs sizes

simplify :: Float -> Integer -> [Float]
simplify size divisor = unfoldr (\s -> if s < 1 then Nothing else Just (s, s / diver)) size
  where
    diver = fromIntegral divisor

prettyPrint :: [(FilePath, Integer)] -> IO ()
prettyPrint pathData = mapM_ dirInfo pathData
  where
    dirInfo (a, b) = putStrLn $ fmtSize b ++ "\t" ++ a
    fmtSize s = let (size, unit) = case simplify (fromInteger s) 1024 of
                                     [] -> (fromInteger s, "B")
                                     xs -> last $ zip xs units
                in show (floor size) ++ " " ++ unit
    units   = ["", "B", "MB", "GB", "TB", "PB"]
    unitsSI = ["B", "MiB", "GiB", "TiB", "PiB"]

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Usage: pdu <path1> <path2> ... <pathN>"
            paths -> mapM_ (getPathSize >=> prettyPrint) paths
