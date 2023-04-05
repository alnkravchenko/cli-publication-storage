module FilesystemUtils where

import System.IO (IOMode (WriteMode), hClose, hFileSize, hPutStrLn, openFile)

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
writeToFile filePath contents  = do
  handle <- openFile filePath WriteMode
  hPutStrLn handle contents
  hClose handle

writeToTXT :: String -> String -> IO ()
writeToTXT = writeToFile

writeToHTML :: String -> String -> IO ()
writeToHTML filePath contents  = do
  handle <- openFile filePath WriteMode
  hPutStrLn handle contents
  hClose handle
