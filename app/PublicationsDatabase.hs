{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module PublicationsDatabase where

import Data.Data   (Data)
import Publication (Publication)

type Path = String
type Fields = [String]
type ID = Int
type TableName = String

data Entity a = Entity { id  :: ID
                       , obj :: a
                       }
  deriving (Eq, Show)

newtype DBConfig
  = DBConfig { path :: Path }

newtype Database
  = CreateDBConnection { config :: DBConfig }

instance Show Database where
  show :: Database -> String
  show (CreateDBConnection (DBConfig path)) = path

class CRUDUtils a b where
  createTable :: a -> String -> Fields -> a
  insert :: a -> TableName -> Entity b -> a
  delete :: a -> TableName -> ID -> a
  update :: a -> TableName -> ID -> Entity b -> a
  select :: a -> TableName -> ID -> Maybe (Entity b)
  selectAll :: a -> TableName -> Maybe [Entity b]

-- HINT: use flip to reverse function arguments order
-- flip :: (a -> b -> c) -> b -> a -> c

instance (Data b, Show b) => CRUDUtils Database b where
  -- create new file
  createTable :: Database -> String -> Fields -> Database
  createTable db name fields = db
  insert :: Database -> TableName -> Entity b -> Database
  insert db table newPub = db
  delete :: Database -> TableName -> ID -> Database
  delete db table pubID = db
  update :: Database -> TableName -> ID -> Entity b -> Database
  update db table pubID newPub = db
  select :: Database -> TableName -> ID -> Maybe (Entity b)
  select db table pubID = Nothing
  selectAll :: Database -> TableName -> Maybe [Entity b]
  selectAll db table = Nothing

