{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Main where
-- See dependencies in Package.yml:
       -- base >= 4.7 && < 5
       -- lens
       -- lens-aeson
       -- bytestring
       -- text
       -- http-conduit
--------------------- Package ----------------------------
--------- 1 : Network.HTTP.Simple : https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md ---------
--Simplified interface for common HTTP client interactions.
--Tutorial available at https://haskell-lang.org/library/http-client
import           Network.HTTP.Simple            ( httpBS, getResponseBody )
import           Control.Monad (when)
import           Data.Functor
import           Data.List
import           GHC.Generics

-- 3-------
--Open a web browser from Haskell. Currently BSD, Linux, OS X and Windows are supported.#--
--https://hackage.haskell.org/package/open-browser
import           Web.Browser (openBrowser)
import           Control.Lens
import           Data.Aeson.Lens
import           System.IO
import           Control.Monad
import           Data.Text                      ( Text )
--------- 2 : For preview ---------
--This package comes "Batteries Included" with many useful lenses
--for the types commonly used from the Haskell Platform, and with
--tools for automatically generating lenses and isomorphisms for user-supplied data types.
import           Control.Lens                   ( preview )
import           Data.Aeson.Lens                ( key, _String )
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as LB

import qualified Data.Text.IO                  as TIO
import qualified Data.String                   as String

-- stack --resolver lts-8.12 script
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Text                     as T

import Data.Aeson(Value(String))
import Data.Text(unpack)


------ This function is responsible
readFileFunc ::  FilePath -> IO ()
readFileFunc filePath  = do
    --- Read the file which in the parameter----
    --- Open file  ReadMore-----
    handle <- openFile filePath ReadMode
    --- get all data -----
    contents <- hGetContents handle
    --- Close File -----
    putStrLn (createNewLine  [contents])
    hClose handle

--------- 3 - get the Json Datei ---------
--This package comes "Batteries Included" with many useful lenses
--for the types commonly used from the Haskell Platform, and with
--tools for automatically generating lenses and isomorphisms for user-supplied data types.
baseApi url  = do
  res <-  httpBS  url
  return (getResponseBody res)

----- This function is needed in order to add new Line using "\n" ----------
createNewLine [] = ""
createNewLine ( x:[] ) = x
createNewLine ( x:xs ) = x ++  "\n" ++ createNewLine xs

----- get new Confirmed cases from API ----------
--getNewConfirmed :: BS.ByteString -> Maybe Integer
--getNewConfirmed = preview (key "Global" .key "NewConfirmed"  . _Integer)

----- get Date  from API ----------
getDate :: BS.ByteString -> Maybe Text
getDate = preview (key "Date" . _String)

----- get TotalConfirmed cases from API ----------
getSummary field = preview (key "Global" .key field . _Integer)

--getSummaryAllCountries :: BS.ByteString -> Maybe Text
--getSummaryAllCountries = preview (key "Countries" . key "Country" . _String)

getCountries :: BS.ByteString -> Maybe Array
getCountries = preview (key "Countries" . _Array)


------ This function is responsible for adding new summaryCountry
saveListCountry file listCountry  = do
    --- Read the file which in the parameter----
    x <- readFile file
    -------- Save the rest ------
    length x `seq` writeFile file listCountry
    -------- Add new line --------
    appendFile file x
    return x

------- 1 -------
myhead :: [a] -> a
myhead [] = error "Empty!"
myhead (x:xs) = x

getString :: Value -> Maybe String
getString (String t) = Just (unpack t)
getString _ = Nothing
data Countries = Countries {
                        countryName :: !Text,
                      --  countryCode :: !Text,
                      --  provinceCountry :: !Text,
                      --  city :: !Text,
                      --  cityCode :: !Text,
                        confirmed :: Int,
                        deaths :: Int,
                        recovered :: Int,
                        active :: Int,
                        date :: !Text
                        }

instance FromJSON Countries where
    parseJSON = withObject "Countries" $ \v -> Countries
        <$> v .: "Country"
      --  <*> v .: "CountryCode"
      --  <*> v .: "Province"
     --   <*> v .: "City"
     --   <*> v .: "CityCode"
        <*> v .: "Confirmed"
        <*> v .: "Deaths"
        <*> v .: "Recovered"
        <*> v .: "Active"
        <*> v .: "Date"

--instance FromJSON CountryList where
  --parseJSON (Object v) = CountryList <$> v .: "Country" <*> v .: "NewConfirmed" <*> v .: "TotalConfirmed" <*> v .: "NewDeaths" <*> v .: "TotalDeaths" <*> v .: "NewRecovered" <*> v .: "TotalRecovered" <*> v .: "Date"
  --parseJSON _ = mzero

--instance FromJSON CountryList where
  --  parseJSON = withObject "CountryList" $ \v -> CountryList
    -- <$> v .: "Country"
    -- <*> v .: "NewConfirmed"
    -- <*> v .: "TotalConfirmed"
    -- <*> v .: "NewDeaths"
    -- <*> v .: "TotalDeaths"
    -- <*> v .: "NewRecovered"
    -- <*> v .: "TotalRecovered"
    -- <*> v .: "Date"


data CountryListSummary = CountryListSummary { countryListSummaryElement :: CountryList } deriving (Show, Generic)

instance FromJSON CountryListSummary where
    parseJSON = withObject "CountryListSummary" $ \v -> CountryListSummary
        <$> v .: "Countries"


data CountryList = CountryList {
                        countryNameForList :: !Text,
                        newConfirmedForList :: Int,
                        totalConfirmedForList :: Int,
                        newDeathsForList :: Int,
                        totalDeathsForList :: Int,
                        newRecoveredForList :: Int,
                        totalRecoveredForList :: Int,
                        dateForList :: !Text
                        }deriving (Show, Generic)

instance FromJSON CountryList where
      parseJSON = withObject "CountryList" $ \v -> CountryList
          <$> v .: "Country"
          <*> v .: "NewConfirmed"
          <*> v .: "TotalConfirmed"
          <*> v .: "NewDeaths"
          <*> v .: "TotalDeaths"
          <*> v .: "NewRecovered"
          <*> v .: "TotalRecovered"
          <*> v .: "Date"

rangeCorona :: [a] -> Int -> Int -> [a]
rangeCorona l i j = take (j-i+1) (drop i l)

confirmedNumber :: Maybe (Text, Int, Int, Int, Int, Text) -> Int
confirmedNumber Nothing     = 0
confirmedNumber (Just (_, v, _, _, _, _)) = v

deathNumber :: Maybe (Text, Int, Int, Int, Int, Text) -> Int
deathNumber Nothing     = 0
deathNumber (Just (_, _, v, _, _, _)) = v

recoveredNumber :: Maybe (Text, Int, Int, Int, Int, Text) -> Int
recoveredNumber Nothing     = 0
recoveredNumber (Just (_, _, _, v, _, _)) = v

activeNumber :: Maybe (Text, Int, Int, Int, Int, Text) -> Int
activeNumber Nothing     = 0
activeNumber (Just (_, _, _, _, v, _)) = v

menu:: IO ()
menu = do
    putStrLn "\n--------------- Welcome  ---------------"
    -- Menu
    putStrLn "1 - About the project"
    putStrLn "2 - Api- Coronavirus COVID19 API Documentation"

    putStrLn "\n------------ Projekt  --------------------"
    putStrLn "3 - Show all countries affected by the virus and search for countries only"
    putStrLn "4 - Show a summary of new and total cases in the world, updated daily."
    putStrLn "5 - Show a summary of new and total cases per country, updated daily."
    putStrLn "6 - Show all cases (confirmed,recovered,deaths) for a country from the first recorded case."
    putStrLn "7 - Filter by a certain date all cases (confirmed,recovered,deaths) for a country from the first recorded case on."
    putStrLn "8 - Filter From - To date all cases (confirmed,recovered,deaths) for a country from the first recorded case on"
    putStrLn "---------------------------------------------"
    putStrLn "\n--------------- Autor: Taher ben sassi  ---------------"
    choice <- readLn
    case choice of
    -- //  Start
      1 -> do
          -- Information about our Group
          putStrLn "---------------------------------------------"
          readFileFunc "documentation/app.txt"
      2 -> do
          putStrLn "Are you sure you want to open your browser? (y/n)"
          choiceLink <- getChar
          case choiceLink of
             'y' -> do
                   -- Open a Link from Browser
                   openBrowser "https://documenter.getpostman.com/view/10808728/SzS8rjbc?version=latest" >>= print
             'n' -> do
                   putStrLn "Thanks !"
    -- // End


    -- // Projekt End
      3 -> do
          putStrLn "1- Show all countries affected by the virus ?"
          putStrLn "2- Check if specific country is affected?"
          choiceCountry <- readLn
          jsonCountries <- baseApi  "https://api.covid19api.com/countries"
          {- Note [^..]
          ~~~~~~~~~~~~~~~~~~~~~~
           --//The ^.. is used from the lens package, which is a synonym for toListOf.
          --//The ^.. operator will take the value on the left hand side (jsonCountries)
          --//and apply the Fold on the right to it, collecting the results into a list.
          -}
          let countries = jsonCountries^..values.key "Country"._String
          case choiceCountry of
          -- // Anleitung Start
            1 -> do
                mapM_ print countries
            2 -> do
                -- Information about our Group
                putStrLn "---------------------------------------------"
                putStrLn "Please enter the country name! In English"
                choiceCountry <- getLine
                 -- Solve Text problem --> String
                let searchedCountry =  map (\x -> (elemIndices x  countries)) [String.fromString choiceCountry]
                {- Here are a few countries that have not reported any cases of the coronavirus so far:
                Kiribati
                Marshall Islands
                Micronesia
                Nauru
                North Korea
                Palau
                Samoa
                Solomon Islands
                Tonga
                Turkmenistan
                Vanuatu
                -}
                let message =
                      if  null (searchedCountry!!0)
                      then "The country you were looking for is NOT infected.  D or The country does not exist"
                      else "The country you were looking for is unfortunately infected. :( "
                print message
      4 -> do
          summaryVirusJson <- baseApi  "https://api.covid19api.com/summary"
          putStrLn "0- Summary"
          putStrLn "1- New and all confirmed cases (Confimed)"
          putStrLn "2- New and all healthy cases (Recoverd)"
          putStrLn "3- New and all death cases (Death)"
          choiceSummary <- readLn
          case choiceSummary of
              0 -> do
                   putStrLn "---------------------------------------------"
                   case getDate summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just date -> putStrLn ("Date: " ++  show date)
                   case getSummary "TotalConfirmed" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just totalConfirmed -> putStrLn ("Coronavirus cases: " ++  show totalConfirmed)
                   case getSummary "NewConfirmed" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just newConfirmed -> putStrLn ("New infections: + " ++  show newConfirmed)

                   case getSummary "NewRecovered"  summaryVirusJson  of
                       Nothing   -> putStrLn "Error - Empty "
                       Just newRecovered -> putStrLn ("New Healthy Again: + " ++  show newRecovered)
                   case getSummary "TotalRecovered" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just totalRecovered -> putStrLn ("Totally Healthy Again :  " ++  show totalRecovered)

                   case getSummary "NewDeaths"  summaryVirusJson  of
                        Nothing   -> putStrLn "Error - Empty "
                        Just newDeaths -> putStrLn ("New deaths: + " ++  show newDeaths)
                   case getSummary "TotalDeaths" summaryVirusJson of
                        Nothing   -> putStrLn "Error - Empty "
                        Just totalDeaths -> putStrLn ("Total deaths :  " ++  show totalDeaths)

              1 -> do
                   putStrLn "---------------------------------------------"
                   case getDate summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just date -> putStrLn ("Date: " ++  show date)
                   case getSummary "TotalConfirmed" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just totalConfirmed -> putStrLn ("Coronavirus cases: " ++  show totalConfirmed)
                   case getSummary "NewConfirmed" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just newConfirmed -> putStrLn ("New infections:: + " ++  show newConfirmed)
              2 -> do
                   putStrLn "---------------------------------------------"
                   case getDate summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just date -> putStrLn ("Date: " ++  show date)

                   case getSummary "NewRecovered"  summaryVirusJson  of
                       Nothing   -> putStrLn "Error - Empty "
                       Just newRecovered -> putStrLn ("New Healthy Again: + " ++  show newRecovered)

                   case getSummary "TotalRecovered" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just totalRecovered -> putStrLn ("Totally Healthy Again : + " ++  show totalRecovered)
              3 -> do
                   putStrLn "---------------------------------------------"
                   case getDate summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just date -> putStrLn ("Date: " ++  show date)

                   case getSummary "NewDeaths"  summaryVirusJson  of
                       Nothing   -> putStrLn "Error - Empty "
                       Just newDeaths -> putStrLn ("New deaths: + " ++  show newDeaths)

                   case getSummary "TotalDeaths" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Empty "
                       Just totalDeaths -> putStrLn ("Total deaths : + " ++  show totalDeaths)
      5 -> do
          putStrLn "---------------------------------------------"
          summaryVirusJson <- baseApi  "https://api.covid19api.com/summary"
          putStrLn "---------------------------------------------"
          case getDate summaryVirusJson of
               Nothing   -> putStrLn "Error - Empty "
               Just date -> putStrLn ("Date: " ++  show date)

           -- // @toDo Fix Nothing Problem
          let countryList = case getCountries summaryVirusJson of
                                           Just n -> n
          let listCountryLine = V.toList countryList
          --mapM_ print  listCountryLine
          mapM_ print  listCountryLine
          --print $ map (\x -> (x,elemIndex x listCountryLine)) needles

          let fileToAdd =  Data.List.tail $ Data.List.foldl (\x y -> x ++ "," ++ show y) "" listCountryLine

          putStrLn "---------------------------------------------"
          putStrLn "Do you want to save the file to file (y/n)? "
          choiceFile <- getChar
          case choiceFile of
              'y' -> do
                    putStrLn "---------------------------------------------"
                    when (length countryList > 0) $
                        writeFile "data/summaryCountry.txt" fileToAdd
                    putStrLn "File is saved !"
              'n' -> do
                    putStrLn "---------------------------------------------"
                    putStrLn "Thanks !"

      {-6 -> do
          putStrLn "---------------------------------------------"
          summaryVirusJson <- baseApi  "https://api.covid19api.com/summary"
          putStrLn "---------------------------------------------"
          case getDate summaryVirusJson of
               Nothing   -> putStrLn "Error - Leer "
               Just date -> putStrLn ("Date: " ++  show date)

           -- // @toDo Fix Nothing Problem
          let countryList = case getCountries summaryVirusJson of
                                             Just n -> n
          let needles = ["afghanistan"]
          let listCountryLine = V.toList countryList
          --mapM_ print  listCountryLine
          print $ map (\x -> (x,elemIndex x listCountryLine)) needles
          --print $ map (\x -> (x,elemIndex x listCountryLine)) needles
            -}

      6 -> do
         putStrLn "---------------------------------------------"
         putStrLn "Please enter the country name! In English "
         searchedCountry <- getLine
         let apiUrl = "https://api.covid19api.com/dayone/country/" ++ searchedCountry


         countryVirusJson <- baseApi (String.fromString apiUrl)

         --case eitherDecodeStrict' countryVirusJson of
           --  Left e -> error e
           --  Right colors -> print $ map [countryName,countryCode] colors
         --print $  (decode countryVirusJson :: Maybe Countries)
         --let listCountryLine = V.toList countryVirusJson
         let countryInfo = case eitherDecodeStrict' countryVirusJson of
                     Left e -> error e
                     Right countries -> map (\c -> (countryName c,
                                                    confirmed c,
                                                    deaths c,
                                                    recovered c,
                                                    active c,
                                                    date c)) countries

         putStrLn "('Country Name','Confirmed','Deaths','Healthy again','Active','Date')"
         putStrLn "------------------------------------------------------------------------"
         mapM_ print countryInfo

         --print (decode countryVirusJson :: Maybe Countries)
         putStrLn "------------------------------------------------------------------------"

         --putStrLn "Filter by Date (von ---> bis)"
         putStrLn "Do you want to save the file to file (y/n)? "
         choiceFile <- getChar
         case choiceFile of
               'y' -> do
                    --print "Bitte geben Sie den Dateinamen ein !, zum Beispiel Deutschland"
                    --fileNameEnter <- getLine
                    let fileName = "data/countries/country.txt"
                        -- // @toDo Fix new File Country
                    writeFile fileName  (show countryInfo)
                    putStrLn "File is saved !"
               'n' -> do
                    putStrLn "---------------------------------------------"
                    putStrLn "Thanks !"

      7 -> do
          putStrLn "Please enter the country name! In English "
          searchedCountry <- getLine
          let apiUrl = "https://api.covid19api.com/dayone/country/" ++ searchedCountry
          -- // @toDo Von-bis Filer
          putStrLn "Please enter the date ! Y-M-T example 2020-03-04 "
          startDate <- getLine


          countryVirusJson <- baseApi (String.fromString apiUrl)
          let countryDate = case eitherDecodeStrict' countryVirusJson of
                     Left e -> error e
                     Right countries -> map (\c -> (date c)) countries

          let countryInfo = case eitherDecodeStrict' countryVirusJson of
                               Left e -> error e
                               Right countries -> map (\c -> (countryName c,
                                                              confirmed c,
                                                              deaths c,
                                                              recovered c,
                                                              active c,
                                                              date c)) countries
          let startDateTime =  startDate ++ "T00:00:00Z"
          let seachedDateText = T.pack startDateTime
          let indexSearchedElement = map (\x -> (elemIndices x  countryDate)) [seachedDateText]
          let searchedByDate = countryInfo^? element (indexSearchedElement!!0!!0)
          putStrLn "('Country Name','Confirmed','Deaths','Healthy again','Active','Date')"
          print searchedByDate

      8 -> do
          putStrLn "Please enter the country name! In English "
          searchedCountry <- getLine
          let apiUrl = "https://api.covid19api.com/dayone/country/" ++ searchedCountry
          -- // @toDo Von-bis Filer
          putStrLn "Please enter the start date ! Format: y-m-t example 2020-03-04 "
          startDate <- getLine
          putStrLn "Please enter the end date ! Format: y-m-t example 2020-03-04"
          endDate <- getLine

          countryVirusJson <- baseApi (String.fromString apiUrl)
          let countryDate = case eitherDecodeStrict' countryVirusJson of
                   Left e -> error e
                   Right countries -> map (\c -> (date c)) countries

          let countryInfo = case eitherDecodeStrict' countryVirusJson of
                             Left e -> error e
                             Right countries -> map (\c -> (countryName c,
                                                            confirmed c,
                                                            deaths c,
                                                            recovered c,
                                                            active c,
                                                            date c)) countries
          -- // @toDo condition check empty and more
          let endDateTime =  endDate ++ "T00:00:00Z"
          let seachedendDateText = T.pack endDateTime

          let startDateTime =  startDate ++ "T00:00:00Z"
          let seachedStartDateText = T.pack startDateTime

          let indexSearchedElementStart = map (\x -> (elemIndices x  countryDate)) [seachedStartDateText]
          let startCoronaObject = countryInfo^? element (indexSearchedElementStart!!0!!0)

          let indexSearchedElementEnd = map (\x -> (elemIndices x  countryDate)) [seachedendDateText]
          let endCoronaObject = countryInfo^? element (indexSearchedElementEnd!!0!!0)

          putStrLn "('Country Name','Confirmed','Deaths','Healthy again','Active','Date')"
          --mapM_ print $ rangeCorona countryInfo (indexSearchedElementStart!!0!!0) (indexSearchedElementEnd!!0!!0)

          let rangeDays = (indexSearchedElementEnd!!0!!0-indexSearchedElementStart!!0!!0+1)
          --print startCoronaObject
          let confirmedStart =  confirmedNumber startCoronaObject
          let deathStart =  deathNumber startCoronaObject
          let recovredStart =  recoveredNumber startCoronaObject
          let activeStart =  activeNumber startCoronaObject

          let confirmedEnd =  confirmedNumber endCoronaObject
          let deathEnd  =  deathNumber endCoronaObject
          let recovredEnd =  recoveredNumber endCoronaObject
          let activeEnd =  activeNumber endCoronaObject

          let diffConfiremd = confirmedEnd - confirmedStart
          let diffDeath = deathEnd - deathStart
          let diffRecovory = recovredEnd - recovredStart
          let diffActive = activeEnd - activeStart

          putStrLn ("From " ++ startDate ++ " to " ++ endDate ++ " are " ++ show rangeDays)
          mapM_ print $ rangeCorona countryInfo (indexSearchedElementStart!!0!!0) (indexSearchedElementEnd!!0!!0)
          putStrLn ("---------------------------------------------------------------------------")
          putStrLn ("On " ++ startDate ++ " was " ++ show confirmedStart ++ " Confirmed !! but on " ++ endDate ++" are " ++ show confirmedEnd ++ " D.h: "  ++ show (fromEnum diffConfiremd) ++ "+" )
          putStrLn ("---------------------------------------------------------------------------")
          putStrLn ("On " ++ startDate ++ " was " ++ show deathStart ++ " Deaths  !! but on " ++ endDate ++" are " ++ show deathEnd ++ " D.h: "  ++ show (fromEnum diffDeath) ++ "+" )
          putStrLn ("---------------------------------------------------------------------------")
          putStrLn ("On " ++ startDate ++ " was " ++ show recovredStart ++ " Healthy again  !! but on " ++ endDate ++" are " ++ show recovredEnd  ++ " D.h: "  ++ show (fromEnum diffRecovory) ++ "+")
          putStrLn ("---------------------------------------------------------------------------")
          putStrLn ("On " ++ startDate ++ " was " ++ show activeStart ++ " Active  !! but on " ++ endDate ++" are " ++ show activeEnd ++ " D.h: "  ++ show (fromEnum diffActive) ++ "+" )
      otherwise -> do
          putStrLn "Please select a number from 1 to 6"
-- Menu
main :: IO ()
main = do
  menu