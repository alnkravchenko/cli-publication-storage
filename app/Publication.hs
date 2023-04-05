{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
module Publication where

import Data.Data (Data)
import Helpers   (City, Creator, Publisher)

data Publication
  = Book { creator   :: Creator
         , title     :: String
         , city      :: City
         , publisher :: Publisher
         , year      :: Integer
         }
  | Article { creator   :: Creator
            , title     :: String
            , publisher :: Publisher
            , year      :: Integer
            }
  | Summary { creator   :: Creator
            , title     :: String
            , publisher :: Publisher
            , city      :: City
            , year      :: Integer
            }
  deriving (Data)

instance Show Publication where
  show :: Publication -> String
  show (Book creator title city publisher year) = concat [show creator, " -- '", title, "'; ", show publisher, " ", show city, ", ", show year]
  show (Article creator title publisher year) = concat [show creator, " -- '", title, "'; ", show publisher, ", ", show year]
  show (Summary creator title publisher city year) = concat [show creator, " -- '", title, "'; ", show publisher, " ", show city, ", ", show year]
