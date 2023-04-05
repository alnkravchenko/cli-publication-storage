{-# LANGUAGE InstanceSigs #-}
module CLI where

import Data.Data            (Data)
import Data.Set             (showTree)
import FilesystemUtils      (createFile)
import Helpers              (Person (Person), splitOn)
import PublicationsDatabase
    (CRUDUtils (..), DBConfig (..), Database (..), Entity (..), ID, TableName)
import PublicationsService  (PubStorageUtils (..), PublicationsService (..))
import System.Directory     (createDirectoryIfMissing)

type Type = String
type Fields = [String]

data Command
  = GetCategory TableName String
  | SearchByAuthor TableName String String
  | SearchByAuthorExcl TableName String Stringw
  | FindAllPublishingHouses TableName
  | FindAllJournals TableName
  | FindAllConferences TableName
  | GetStatsByPublicationType TableName String
  | CreateTable TableName
  | InsertPublication TableName Type Fields
  | DeletePublication TableName ID
  | UpdatePublication TableName ID Fields
  | SelectPublication TableName ID
  | Help
  deriving (Show)

newtype CLI
  = CLI { cliName :: String }

instance Show CLI where
  show :: CLI -> String
  show (CLI name) = "You are using '" ++ name ++ "' CLI tool\nPlease type 'help' to see all commands"

createDB :: (PubStorageUtils a, Show a) => String -> IO a
createDB folderPath = do
  createDirectoryIfMissing True folderPath
  return (createStorage folderPath)

parseCommand :: [String] -> Command
parseCommand ["get-category", tableName, title] = GetCategory tableName title
parseCommand ["search-by-author", tableName, firstName, lastName, excludeFlag] =
  case excludeFlag of
    ""   -> SearchByAuthor tableName firstName lastName
    "-e" -> SearchByAuthorExcl tableName firstName lastName
parseCommand ["find-all-publishing-houses", tableName] = FindAllPublishingHouses tableName
parseCommand ["find-all-journals", tableName] = FindAllJournals tableName
parseCommand ["find-all-conferences", tableName] = FindAllConferences tableName
parseCommand ["get-stats-by-publication-type", tableName, publicationType] = GetStatsByPublicationType tableName publicationType
parseCommand ["create-table", tableName] = CreateTable tableName
parseCommand ["insert", tableName, pubType, fields] = InsertPublication tableName pubType (splitOn "," fields)
parseCommand ["delete", tableName, id] = DeletePublication tableName (read id)
parseCommand ["update", tableName, id, fields] = UpdatePublication tableName (read id) (splitOn "," fields)
parseCommand ["select", tableName, id] = SelectPublication tableName (read id)
parseCommand _ = Help

selectAllFromTable :: (Data a, Show a) => PublicationsService a -> TableName -> [a]
selectAllFromTable storage tableName =
  let db = database storage
      maybeData = selectAll db tableName
      parsedData = case maybeData of
        Just val -> map obj val
        Nothing  -> []
   in parsedData

-- implementation for PublicationsService
processCommand :: (Data a, Show a) => PublicationsService a -> Command -> IO String
processCommand storage (GetCategory tableName title) = do
  let parsedData = selectAllFromTable storage tableName
  let filledStorage = StorePublications parsedData (database storage)
  return (show $ getCategoryByTitle filledStorage title)
processCommand storage (SearchByAuthor tableName firstName lastName) = do
  let parsedData = selectAllFromTable storage tableName
  let filledStorage = StorePublications parsedData (database storage)
  return (show $ searchByAuthor filledStorage (Person firstName lastName))
processCommand storage (SearchByAuthorExcl tableName firstName lastName) = do
  let parsedData = selectAllFromTable storage tableName
  let filledStorage = StorePublications parsedData (database storage)
  return (show $ searchByAuthorExcl filledStorage (Person firstName lastName))
processCommand storage (FindAllPublishingHouses tableName) = do
  let parsedData = selectAllFromTable storage tableName
  let filledStorage = StorePublications parsedData (database storage)
  return (showTree $ findAllPublishingHouses filledStorage)
processCommand storage (FindAllJournals tableName) = do
  let parsedData = selectAllFromTable storage tableName
  let filledStorage = StorePublications parsedData (database storage)
  return (showTree $ findAllJournals filledStorage)
processCommand storage (FindAllConferences tableName) = do
  let parsedData = selectAllFromTable storage tableName
  let filledStorage = StorePublications parsedData (database storage)
  return (showTree $ findAllConferences filledStorage)
processCommand storage (GetStatsByPublicationType tableName publicationType) = do
  let parsedData = selectAllFromTable storage tableName
  let filledStorage = StorePublications parsedData (database storage)
  return (getStatsByPublicationType filledStorage publicationType)
processCommand storage (CreateTable tableName) = do createTable storage tableName
processCommand _ Help = do
  return
    "Available commands:\n\
    \  quit - close CLI\n\
    \  get-category [table name] [title]\n\
    \  search-by-author [table name] [first name] [last name] [exclude]\n\
    \  find-all-publishing-houses [table name]\n\
    \  find-all-journals [table name]\n\
    \  find-all-conferences [table name]\n\
    \  get-stats-by-publication-type [table name] [publication type]"
