{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
module Publication where

import Data.Data (Data)
import Helpers
    ( City (City)
    , Creator
    , Pages (Pages)
    , Publisher (Conference, Journal, PublishingHouse)
    , splitOn
    , strToCountry
    , strToCreator
    )

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

class Mappable a where
  createFromMap :: [String] -> a

instance Mappable Publication where 
  createFromMap :: [String] -> Publication
  createFromMap ["Book", creator, title, city, publisher, year] = Book { creator = strToCreator creator, title = title, city = parsedCity, publisher = PublishingHouse publisher, year = read year }
    where parsedCityData = splitOn ',' city
          parsedCity = City (head parsedCityData, strToCountry $ last parsedCityData)

  createFromMap ["Article", creator, title, publisher, year] = Article { creator = strToCreator creator, title = title, publisher = parsedPublisher, year = read year }
    where parsedPublisherData = splitOn ',' publisher
          pages = Pages (read $ parsedPublisherData !! 2, read $ last parsedPublisherData)
          parsedPublisher = Journal (head parsedPublisherData) (read $ parsedPublisherData !! 1) pages

  createFromMap ["Summary", creator, title, publisher, city, year] = Summary { creator = strToCreator creator, title = title, publisher = parsedPublisher, city = parsedCity, year = read year }
    where parsedPublisherData = splitOn ',' publisher
          pages = Pages (read $ parsedPublisherData !! 1, read $ last parsedPublisherData)
          parsedPublisher = Conference (head parsedPublisherData) pages
          parsedCityData = splitOn ',' city
          parsedCity = City (head parsedCityData, strToCountry $ last parsedCityData)

instance Show Publication where
  show :: Publication -> String
  show (Book creator title city publisher year) = concat [show creator, " -- '", title, "'; ", show publisher, " ", show city, ", ", show year]
  show (Article creator title publisher year) = concat [show creator, " -- '", title, "'; ", show publisher, ", ", show year]
  show (Summary creator title publisher city year) = concat [show creator, " -- '", title, "'; ", show publisher, " ", show city, ", ", show year]
