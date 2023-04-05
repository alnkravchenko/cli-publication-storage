module FilesystemUtils where

import Data.List (intercalate)
import Helpers   (splitOn)
import System.IO
    ( IOMode (AppendMode, ReadMode, WriteMode)
    , hClose
    , hFileSize
    , hGetContents
    , hPutStrLn
    , openFile
    )

type Row = [String]

createFile :: String -> String -> IO String
createFile folderPath fileName = do
  let fullPath = folderPath ++ "/" ++ fileName
  handle <- openFile fullPath WriteMode
  fileSize <- hFileSize handle
  if fileSize == 0
    then do
      hClose handle
      return (fileName ++ " created")
    else do
      hClose handle
      return (fileName ++ "already exists")

writeToFile :: String -> String -> IO ()
writeToFile filePath content = do
  handle <- openFile filePath AppendMode
  hPutStrLn handle content
  hClose handle

readMyFile :: String -> IO String
readMyFile filePath = do
  handle <- openFile filePath ReadMode
  content <- hGetContents handle
  hClose handle
  return content

writeRowToCSV :: String -> Row -> IO ()
writeRowToCSV filePath row = writeToFile filePath (intercalate "," row)

writeToTXT :: String -> String -> IO ()
writeToTXT = writeToFile

writeToHTML :: String -> String -> IO ()
writeToHTML filePath content = do
  handle <- openFile filePath WriteMode
  hPutStrLn handle content
  hClose handle

readCSV :: String -> IO [Row]
readCSV filePath = do
  fileData <- readMyFile filePath
  let fileLines = lines fileData
  return $ map (splitOn ',') fileLines

deleteRowFromCSV :: String -> Int -> IO ()
deleteRowFromCSV filePath rowIndex = do
  -- read the file and split it into lines
  fileContents <- readFile filePath
  let fileLines = lines fileContents
  -- delete the line at the specified index (if it exists)
  let updatedLines = case splitAt rowIndex fileLines of
                        (before, [])      -> before  -- index out of bounds
                        (before, _:after) -> before ++ after
  -- write the updated file contents back to the file
  writeToFile filePath (unlines updatedLines)
