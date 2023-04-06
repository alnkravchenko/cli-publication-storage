{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
module Helpers where

import Data.Char         (toLower)
import Data.Data         (Data)
import Data.List         (intercalate)

-- task 1
-- Оголосити необхiднi типи даних i класи типiв для роботи з базою даних.
-- Публiкацiї. Зберiгаються данi про публiкацiї:
-- книга (автор/спiвавтори, назва, мiсто, видавництво, рiк)
-- стаття (автор/спiвавтори, назва статтi, назва журналу, рiк, номер журналу, сторiнки)
-- тези доповiдi (автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто,рiк, сторiнки).

data Person = Person { name    :: String
                     , surname :: String
                     }
  deriving (Data, Eq)

instance Show Person where
  show :: Person -> String
  show (Person name surname) = name ++ " " ++surname

newtype Creator
  = Authors [Person]
  deriving (Data)

instance Show Creator where
  show :: Creator -> String
  show (Authors people) =  intercalate "," $ map show people

containsAuthor :: Creator -> Person -> Bool
containsAuthor (Authors authors) person = person `elem` authors

lenAuthors :: Creator -> Int
lenAuthors (Authors authors) = length authors

strToCreator :: String -> Creator
strToCreator str = parsedCreator
  where parsedCreatorData = splitOn ',' str
        toPerson str = Person (head $ words str) (last $ words str)
        parsedCreator = Authors (map toPerson parsedCreatorData)

data Country
  = Ukraine
  | Germany
  | UK
  | USA
  | France
  | Italy
  | Spain
  | Sweden
  | Australia
  | Canada
  deriving (Data, Eq, Show)

strToCountry :: String -> Country
strToCountry str = case map toLower str of
  "ukraine"   -> Ukraine
  "germany"   -> Germany
  "uk"        -> UK
  "usa"       -> USA
  "france"    -> France
  "italy"     -> Italy
  "spain"     -> Spain
  "sweden"    -> Sweden
  "australia" -> Australia
  "canada"    -> Canada
  _           -> Ukraine

newtype City
  = City (String, Country)
  deriving (Data)

instance Show City where
  show :: City -> String
  show (City (city, country)) = concat [city, ", ", show country]


type JournalNumber = Integer

newtype Pages
  = Pages (Integer, Integer)
  deriving (Data)

instance Show Pages where
  show :: Pages -> String
  show (Pages p) = concat ["(p. ", show (fst p),"-", show (snd p),")"]

data Publisher
  = PublishingHouse { publisherName :: String
                    }
  | Journal { publisherName :: String
            , journalNum    :: JournalNumber
            , journalPages  :: Pages
            }
  | Conference { publisherName :: String
               , confPages     :: Pages
               }
  deriving (Data)


instance Show Publisher where
  show :: Publisher -> String
  show (PublishingHouse publisherName) = concat ["Publishing House ", "'", publisherName, "'"]
  show (Journal publisherName journalNum journalPages) = concat ["Journal ", "'", publisherName, "' ", "#", show journalNum, " ", show journalPages]
  show (Conference publisherName confPages) = concat ["Conference ", "'", publisherName, "' ", show confPages]

instance Eq Publisher where
  (==) :: Publisher -> Publisher -> Bool
  (==) (PublishingHouse name1) (PublishingHouse name2) = name1 == name2
  (==) (Journal name1 _ _) (Journal name2 _ _)         = name1 == name2
  (==) (Conference name1 _) (Conference name2 _)       = name1 == name2

instance Ord Publisher where
  compare :: Publisher -> Publisher -> Ordering
  (PublishingHouse name1) `compare` (PublishingHouse name2) = name1 `compare` name2
  (Journal name1 _ _) `compare` (Journal name2 _ _)         = name1 `compare` name2
  (Conference name1 _) `compare` (Conference name2 _)       = name1 `compare` name2


splitOn :: Char -> String -> [String]
splitOn delimiter str =
    let (prefix, suffix) = break (== delimiter) str
    in  case suffix of
            "" -> [prefix]
            _  -> prefix : splitOn delimiter (tail suffix)
