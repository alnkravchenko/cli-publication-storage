module Main where

import CLI                 (CLI (CLI), createDB, parseCommand, processCommand)
import Control.Monad       (join)
import FilesystemUtils     (writeToHTML, writeToTXT)
import Helpers
    ( City (City)
    , Country (Canada, France, USA, Ukraine)
    , Creator (Authors)
    , Pages (Pages)
    , Person (Person)
    , Publisher (Conference, Journal, PublishingHouse)
    )
import Publication         (Publication (Article, Book, Summary))
import PublicationsService
    ( PubStorageUtils (findAllConferences, findAllJournals, findAllPublishingHouses, getCategoryByTitle, getStatsByPublicationType, searchByAuthor, searchByAuthorExcl)
    , PublicationsService (StorePublications)
    )

main :: IO ()
main = do
  putStrLn "Lab 4"
  print (CLI "Publications Database")
  putStrLn "Pick a mode of stdout (0 - on screen, 1 - to txt file, 2 - to html, 3 - no output): "
  modeArg <- getLine
  let mode = read modeArg :: Int
  print mode

  putStrLn "Provide a path to the database folder: "
  storageArg <- getLine
  storage <- createDB storageArg :: IO (PublicationsService Publication)
  putStrLn "Database folder created!"

  -- userInput <- getContents
  -- -- parse
  -- let commands = map (parseCommand . words) (lines userInput)
  -- -- process
  -- let resultsIO = mapM (processCommand storage) commands
  -- -- report
  -- let printResultsIO = fmap (mapM_ putStrLn) resultsIO
  -- let writeResultsIO = fmap (mapM_ (writeToTXT "./data/out.txt")) resultsIO
  -- let htmlResultsIO = fmap (mapM_ (writeToHTML "./data/out.html")) resultsIO
  -- case mode of
  --   0 -> join printResultsIO
  --   1 -> join writeResultsIO
  --   2 -> join htmlResultsIO
  --   3 -> putStrLn ""
