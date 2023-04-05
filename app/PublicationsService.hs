{-# LANGUAGE InstanceSigs #-}
module PublicationsService where

import Data.Data (Data (dataTypeOf, toConstr), cast, showConstr, typeOf)
import Data.List (intercalate)
import Data.Set  (Set, fromList, toList)

import Helpers
    (Person, Publisher (PublishingHouse, publisherName), containsAuthor, lenAuthors)
import Publication          (Publication (Article, Book, Summary))
import PublicationsDatabase (DBConfig (DBConfig), Database (CreateDBConnection))

-- Серед методiв створених класiв має бути хоча б один оператор.
-- Створенi типи даних треба оголосити екземплярами створених та iнших вiдповiдних класiв,
-- зокрема Foldable та Monoid. Данi, оскiльки базова система введення/виведення ще не розглядалась, можна зберiгати у кодi.

data PublicationsService a = StorePublications { publications :: [a]
                                               , database     :: Database
                                               }

instance (Show a) => Show (PublicationsService a) where
  show :: PublicationsService a -> String
  show (StorePublications ps db) =
    let createOrderNums = map (\x -> show x ++ ") ") [1..length ps]
    in "Database folder: " ++ show db ++ "\n" ++ intercalate "\n" (zipWith (++) createOrderNums (map show ps))

instance Semigroup (PublicationsService a) where
  (<>) :: PublicationsService a -> PublicationsService a -> PublicationsService a
  (<>) storage (StorePublications [] _)                     = storage
  (<>) (StorePublications [] _) storage                     = storage
  (<>) (StorePublications p1 db) (StorePublications p2 _) = StorePublications (p1 ++ p2) db

instance Monoid (PublicationsService a) where
  mempty :: PublicationsService a
  mempty = StorePublications [] (CreateDBConnection (DBConfig ""))
  mappend :: PublicationsService a -> PublicationsService a -> PublicationsService a
  mappend = (<>)

instance Foldable PublicationsService where
  foldr :: (a -> b -> b) -> b -> PublicationsService a -> b
  foldr f z (StorePublications ps _) = foldr f z ps
  length :: PublicationsService a -> Int
  length (StorePublications ps _) = length ps
  elem :: Eq a => a -> PublicationsService a -> Bool
  elem x (StorePublications ps _) = x `elem` ps

-- Визначне функцiї для:
-- 2.1 визначення до якого типу вiдноситься публiкацiя з певною назвою (книга, стаття або тези);
-- 2.2 пошуку усiх статей (книг, тез) вказаного автора;
-- 2.3 пошуку усiх одноосiбних публiкацiй вказаного автора;
-- 2.4 пошуку усiх видавництв, журналiв, конференцiй;
-- 2.5 статистика по базi за типом публiкацiй;

class PubStorageUtils a where
  createStorage :: String -> a
  getCategoryByTitle :: a -> String -> [String]
  searchByAuthor :: a -> Person -> a
  searchByAuthorExcl :: a ->  Person -> a
  findAllPublishingHouses :: a -> Set Publisher
  findAllJournals :: a -> Set Publisher
  findAllConferences :: a -> Set Publisher
  getStatsByPublicationType :: a -> String -> String

hasTitle :: Data a => a -> String -> Bool
hasTitle x titleToFind =
  case cast x of
    Just (Book _ title _ _ _)    -> titleToFind == title
    Just (Article _ title _ _)   -> titleToFind == title
    Just (Summary _ title _ _ _) -> titleToFind == title
    _                            -> False

hasAuthor :: Data a => a -> Person -> Bool
hasAuthor x person =
  case cast x of
  Just (Book creator _ _ _ _)    -> containsAuthor creator person
  Just (Article creator _ _ _)   -> containsAuthor creator person
  Just (Summary creator _ _ _ _) -> containsAuthor creator person
  _                              -> False

hasOneCreator :: Data a => a -> Bool
hasOneCreator x =
  case cast x of
  Just (Book creator _ _ _ _)    -> lenAuthors creator == 1
  Just (Article creator _ _ _)   -> lenAuthors creator == 1
  Just (Summary creator _ _ _ _) -> lenAuthors creator == 1
  _                              -> False

getConstructorName :: Data a => a -> String
getConstructorName x =
  let dataType = dataTypeOf x
      constructor = toConstr x
  in showConstr constructor

filterByPublicationType :: Data a => PublicationsService a -> String -> [a]
filterByPublicationType (StorePublications ps _) pubType =
  filter (\x -> getConstructorName x == pubType) ps

getPublisher :: Data a => a -> Publisher
getPublisher x =
  case cast x of
  Just (Book _ _ _ publisherName _)    -> publisherName
  Just (Article _ _ publisherName _)   -> publisherName
  Just (Summary _ _ publisherName _ _) -> publisherName
  _                                    -> PublishingHouse ""

findAllPublishers :: Data a => PublicationsService a -> String -> Set Publisher
findAllPublishers storage pubType =
  let filtered = filterByPublicationType storage pubType
      publishers = map getPublisher filtered
      notEmpty = filter (\x -> not (null $ publisherName x)) publishers
  in fromList notEmpty

instance Data a => PubStorageUtils (PublicationsService a) where
  createStorage :: String -> PublicationsService a
  createStorage dbPath = StorePublications [] (CreateDBConnection (DBConfig dbPath))

  getCategoryByTitle :: Data a => PublicationsService a -> String -> [String]
  getCategoryByTitle (StorePublications ps _) pubTitle =
    let filtered = filter (`hasTitle` pubTitle) ps
    in map getConstructorName filtered

  searchByAuthor :: PublicationsService a -> Person -> PublicationsService a
  searchByAuthor (StorePublications ps db) author =
    let filtered = filter (`hasAuthor` author) ps
    in StorePublications filtered db

  searchByAuthorExcl :: PublicationsService a -> Person -> PublicationsService a
  searchByAuthorExcl (StorePublications ps db) author =
    let filtered = filter (\x -> hasOneCreator x && x `hasAuthor`author) ps
    in StorePublications filtered db

  findAllPublishingHouses :: Data a => PublicationsService a -> Set Publisher
  findAllPublishingHouses storage = findAllPublishers storage "Book"

  findAllJournals :: Data a => PublicationsService a -> Set Publisher
  findAllJournals storage = findAllPublishers storage "Article"

  findAllConferences :: Data a => PublicationsService a -> Set Publisher
  findAllConferences storage = findAllPublishers storage "Summary"

  getStatsByPublicationType :: PublicationsService a -> String -> String
  getStatsByPublicationType storage pubType =
    let filtered = filterByPublicationType storage pubType
        filteredLen = fromIntegral $ length filtered
        totalLen = fromIntegral $ length storage
        amount = ["Amount of ", pubType, ": ", show (length filtered)]
        percentage = ["Percentage from all publications: ", show (round (filteredLen / totalLen  * 100)), "%"]
    in intercalate "\n" $ map concat [amount, percentage]
