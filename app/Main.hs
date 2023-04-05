-- {-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import CLI                 (CLI (CLI), createDB, parseCommand, processCommand)
import Control.Monad       (when)
import Data.Data           (Data)
import Data.List           (intercalate)
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

  let outputFunc = case mode of
        0 -> putStrLn
        1 -> writeToTXT "./data/out.txt"
        2 -> writeToHTML "./data/out.html"
        3 -> (\x -> putStr "")

  putStrLn "Provide a path to the database folder: "
  storageArg <- getLine
  storage <- createDB storageArg :: IO (PublicationsService Publication)
  putStrLn "Database folder created!"
  -- processing
  processInput storage outputFunc


processInput :: (Data a, Show a) => PublicationsService a -> (String -> IO ()) -> IO ()
processInput storage outputFunc = do
  input <- getLine
  when (input /= "quit") $ do
    -- parse
    let command = parseCommand $ words input
    -- process
    resultIO <- processCommand storage command
    -- report
    outputFunc resultIO
    processInput storage outputFunc



-- -- -- Приклади

-- -- summary_1 :: Publication
-- -- summary_1 = Summary (Authors [Person "John" "Doe", Person "Edward" "Snow"]) "Summary title" (Conference "MyConf" (Pages (6, 10))) (City ("New Orlean", USA)) 1978

-- -- article_1 :: Publication
-- -- article_1 = Article (Authors [Person "Denis" "Kazanski"]) "Monads in Haskell" (Journal "Daily FP" 4 (Pages (23, 31))) 2017

-- -- database_1 :: PublicationsService Publication
-- -- database_1 = StorePublications [summary_1, article_1]


-- -- summary_2 :: Publication
-- -- summary_2 = Summary (Authors [Person "John" "Doe", Person "Edward" "Snow"]) "How to Disappear and Reappear with a Sense of Humor" (Conference "MyConf" (Pages (1, 32))) (City ("Toronto", Canada)) 1999

-- -- article_2 :: Publication
-- -- article_2 = Article (Authors [Person "Karl" "Wu"]) "Why OOP?" (Journal "Life or dev" 11 (Pages (27, 28))) 2023

-- -- book_1 :: Publication
-- -- book_1 = Book (Authors [Person "Volodymyr" "Marchenko", Person "Svitlana" "Polyi"]) "A fiction book" (City ("Lviv", Ukraine)) (PublishingHouse "Nash Format") 2020

-- -- book_3 :: Publication
-- -- book_3 = Book (Authors [Person "Volodymyr" "Marchenko"]) "A book 2" (City ("Lviv", Ukraine)) (PublishingHouse "Nash Format") 2020

-- -- database_2 :: PublicationsService Publication
-- -- database_2 = StorePublications [summary_2, article_2, book_1, book_3]

