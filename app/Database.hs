{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database where

import           Data.Data        (Data)
import           Data.List        (elemIndex)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import qualified Data.Maybe
import           FilesystemUtils  (createFile, deleteRowFromCSV, readCSV, writeRowToCSV)
import           Helpers          (splitOn)
import           Publication      (Publication)
import           System.Directory (doesFileExist)

type Path = String
type FieldValue = String
type FieldName = String
type Fields = [String]
type ID = Int
type TableName = String

data Entity = Entity { entityID :: ID
                     , vals     :: Map FieldName String
                     }
  deriving (Eq, Show)

newtype DBConfig
  = DBConfig { path :: Path }

newtype Database
  = CreateDBConnection { config :: DBConfig }

instance Show Database where
  show :: Database -> String
  show (CreateDBConnection (DBConfig path)) = path

class EntityUtils a where
  getField :: a -> FieldName -> Maybe FieldValue
  setField :: a -> FieldName -> FieldValue -> a
  getID :: a -> ID
  mapToEntity :: Map FieldName String -> a

instance EntityUtils Entity where
  getField :: Entity -> String -> Maybe FieldValue
  getField ent fieldName = Map.lookup fieldName fields
    where fields = vals ent
  getID :: Entity -> ID
  getID = entityID
  setField :: Entity -> FieldName -> FieldValue -> Entity
  setField ent fname fval =
    let fieldNames = Map.keys (vals ent)
        newVals = Map.insert fname fval (vals ent)
    in
      if fname `elem` fieldNames
      then Entity (entityID ent) newVals
      else ent
  mapToEntity :: Map FieldName String -> Entity
  mapToEntity m = Entity entityID fields
    where entityID = read $ Data.Maybe.fromMaybe "0" (Map.lookup "ID" m)
          fields = Map.fromList $ tail $ Map.toList m

class CRUDUtils a where
  createTable :: a -> String -> FieldName -> IO String
  isTableExist :: a -> TableName -> IO Bool
  insert :: a -> TableName -> Entity -> IO String
  delete :: a -> TableName -> ID -> IO String
  update :: a -> TableName -> ID -> Entity -> IO String
  select :: a -> TableName -> ID ->  IO (Maybe Entity)
  selectAll :: a -> TableName -> IO (Maybe [Entity])


instance CRUDUtils Database where
  -- create new file for a table records
  createTable :: Database -> String -> FieldName -> IO String
  createTable db name fields = do
    let dbPath = path $ config db
    let tableCreationRes = createFile dbPath (name ++ ".csv")
    writeRowToCSV (dbPath ++ name ++ ".csv") (splitOn ',' fields)
    tableCreationRes

  isTableExist :: Database -> TableName -> IO Bool
  isTableExist db name = do
    let tablePath = path (config db) ++ name ++ ".csv"
    doesFileExist tablePath

  insert :: Database -> TableName -> Entity -> IO String
  insert db table newEnt = do
    let tablePath = path (config db) ++ table ++ ".csv"
    isExist <- isTableExist db table
    if isExist then do
      let values = show (entityID newEnt) : Map.elems (vals newEnt)
      writeRowToCSV tablePath values
      return "Entity added!"
    else
      return "Table does not exist"

  delete :: Database -> TableName -> ID -> IO String
  delete db table entID = do
    let tablePath = path (config db) ++ table ++ ".csv"
    isExist <- isTableExist db table
    if isExist then do
      -- read records from CSV file
      csvData <- readCSV tablePath
      -- get index of line with entity record
      let indices = map head csvData
      let index = elemIndex (show entID) indices
      -- check if record exists
      case index of
        Just n -> do
          deleteRowFromCSV tablePath n
          return "Entity deleted!"
        Nothing -> return "Entity not found!"
    else
      return "Table does not exist"

  update :: Database -> TableName -> ID -> Entity -> IO String
  update db table entID newEnt = do
      -- delete record
      delRes <- delete db table entID
      if delRes == "Entity deleted!" then do
        -- add new record
        instRes <- insert db table newEnt
        if instRes == "Entity added!" then do
          return "Entity updated!"
        else
          return instRes
      else
        return delRes

  select :: Database -> TableName -> ID -> IO (Maybe Entity)
  select db table entID = do
    let tablePath = path (config db) ++ table ++ ".csv"
    isExist <- isTableExist db table
    if isExist then do
      -- read records from CSV file
      csvData <- readCSV tablePath
      -- parse lines
      let fieldNames = head csvData
      let fieldsData = tail csvData
      -- filter data
      let rawFieldData = tail $ head $ filter (\row -> head row == show entID) fieldsData
      -- return record
      let fieldData = Map.fromList $ zip fieldNames rawFieldData
      return $ Just (Entity entID fieldData)
    else
      return Nothing

  selectAll :: Database -> TableName -> IO (Maybe [Entity])
  selectAll db table = do
    let tablePath = path (config db) ++ table ++ ".csv"
    isExist <- isTableExist db table
    if isExist then do
      -- read records from CSV file
      csvData <- readCSV tablePath
      -- parse string to Map
      let fieldNames = head csvData
      let fieldData = tail csvData
      let fieldMap = map (Map.fromList . zip fieldNames) fieldData
      -- parse Map to Entity
      let entities = map mapToEntity fieldMap
      return $ Just entities
    else
      return Nothing
