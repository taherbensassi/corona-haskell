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
myhead [] = error "Leere Liste!"
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
    putStrLn "\n  Gitlab: https://gitlab.beuth-hochschule.de/s79745/haskell-corona-app"
    putStrLn "\n--------------- Anleitung  ---------------"
    -- Menu
    putStrLn "1 - Über Projekt"
    putStrLn "2 - Api- Coronavirus COVID19 API Documentation"

    putStrLn "\n------------ Projekt  --------------------"
    putStrLn "3 - Zeigen Sie alle Länder, die von dem Virus betroffen sind und suchen Sie nur nach Länder"
    putStrLn "4 - Zeigen eine Zusammenfassung der neuen und gesamten Fälle in der Welt, die täglich aktualisiert wird."
    putStrLn "5 - Zeigen eine Zusammenfassung der neuen und gesamten Fälle pro Land, die täglich aktualisiert wird."
    --putStrLn "6 - Suchen -bei Land Name- für eine Zusammenfassung der neuen und gesamten Fälle"
    putStrLn "6 - Zeigen Sie alle Fälle (bestätigt,wieder gesund,Todesfälle) für ein Land ab dem ersten aufgezeichneten Fall an."
    putStrLn "7 - Filter by einem bestimmten Datum alle Fälle (bestätigt,wieder gesund,Todesfälle) für ein Land ab dem ersten aufgezeichneten Fall an."
    putStrLn "8 - Filter  Von - Bis Datum alle Fälle (bestätigt,wieder gesund,Todesfälle) für ein Land ab dem ersten aufgezeichneten Fall an."
    putStrLn "---------------------------------------------"
    choice <- readLn
    case choice of
    -- // Anleitung Start
      0 -> do
          -- Information about our Group
          putStrLn "---------------------------------------------"
          readFileFunc "documentation/team.txt"
      1 -> do
          -- Information about our Group
          putStrLn "---------------------------------------------"
          readFileFunc "documentation/app.txt"
      2 -> do
          putStrLn "Sind Sie sicher, dass Sie Ihren Browser öffnen möchten? (y/n)"
          choiceLink <- getChar
          case choiceLink of
             'y' -> do
                   -- Open a Link from Browser
                   openBrowser "https://documenter.getpostman.com/view/10808728/SzS8rjbc?version=latest" >>= print
             'n' -> do
                   putStrLn "Danke !"
    -- // Anleitung End


    -- // Projekt End
      3 -> do
          putStrLn "1- Zeigen Sie alle Länder an, die vom Virus betroffen sind ?"
          putStrLn "2- Prüfen ob bestimmte Land Betroffen ist?"
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
                putStrLn "Bitte geben Sie den Ländernamen ein! Auf English"
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
                      then "Das Land, das Sie gesucht haben, ist NICHT infiziert.  :D oder Das Land existiert nicht"
                      else "Das Land, das Sie gesucht haben, ist leider infiziert. :( "
                print message
      4 -> do
          summaryVirusJson <- baseApi  "https://api.covid19api.com/summary"
          putStrLn "0- Summary (All Information)"
          putStrLn "1- Neue und alle bestätigte Fälle (Confimed)"
          putStrLn "2- Neue und alle wieder gesund Fälle (Recoverd)"
          putStrLn "3- Neue und alle Todesfälle Fälle (Death)"
          choiceSummary <- readLn
          case choiceSummary of
              0 -> do
                   putStrLn "---------------------------------------------"
                   case getDate summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just date -> putStrLn ("Date: " ++  show date)
                   case getSummary "TotalConfirmed" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just totalConfirmed -> putStrLn ("Coronavirus-Fälle: " ++  show totalConfirmed)
                   case getSummary "NewConfirmed" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just newConfirmed -> putStrLn ("Neuinfektionen: + " ++  show newConfirmed)

                   case getSummary "NewRecovered"  summaryVirusJson  of
                       Nothing   -> putStrLn "Error - Leer "
                       Just newRecovered -> putStrLn ("Neue Wieder Gesund: + " ++  show newRecovered)
                   case getSummary "TotalRecovered" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just totalRecovered -> putStrLn ("Total Wieder Gesund :  " ++  show totalRecovered)

                   case getSummary "NewDeaths"  summaryVirusJson  of
                        Nothing   -> putStrLn "Error - Leer "
                        Just newDeaths -> putStrLn ("Neue Todesfälle: + " ++  show newDeaths)
                   case getSummary "TotalDeaths" summaryVirusJson of
                        Nothing   -> putStrLn "Error - Leer "
                        Just totalDeaths -> putStrLn ("Total Todesfälle :  " ++  show totalDeaths)

              1 -> do
                   putStrLn "---------------------------------------------"
                   case getDate summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just date -> putStrLn ("Date: " ++  show date)
                   case getSummary "TotalConfirmed" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just totalConfirmed -> putStrLn ("Coronavirus-Fälle: " ++  show totalConfirmed)
                   case getSummary "NewConfirmed" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just newConfirmed -> putStrLn ("Neuinfektionen: + " ++  show newConfirmed)
              2 -> do
                   putStrLn "---------------------------------------------"
                   case getDate summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just date -> putStrLn ("Date: " ++  show date)

                   case getSummary "NewRecovered"  summaryVirusJson  of
                       Nothing   -> putStrLn "Error - Leer "
                       Just newRecovered -> putStrLn ("Neue Wieder Gesund: + " ++  show newRecovered)

                   case getSummary "TotalRecovered" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just totalRecovered -> putStrLn ("Total Wieder Gesund : + " ++  show totalRecovered)
              3 -> do
                   putStrLn "---------------------------------------------"
                   case getDate summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just date -> putStrLn ("Date: " ++  show date)

                   case getSummary "NewDeaths"  summaryVirusJson  of
                       Nothing   -> putStrLn "Error - Leer "
                       Just newDeaths -> putStrLn ("Neue Todesfälle: + " ++  show newDeaths)

                   case getSummary "TotalDeaths" summaryVirusJson of
                       Nothing   -> putStrLn "Error - Leer "
                       Just totalDeaths -> putStrLn ("Total Todesfälle : + " ++  show totalDeaths)
      5 -> do
          putStrLn "---------------------------------------------"
          summaryVirusJson <- baseApi  "https://api.covid19api.com/summary"
          putStrLn "---------------------------------------------"
          case getDate summaryVirusJson of
               Nothing   -> putStrLn "Error - Leer "
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
          putStrLn "Wollen Sie die Datei in file speichern (y/n)? "
          choiceFile <- getChar
          case choiceFile of
              'y' -> do
                    putStrLn "---------------------------------------------"
                    when (length countryList > 0) $
                        writeFile "data/summaryCountry.txt" fileToAdd
                    putStrLn "Datei wird gespeichert !"
              'n' -> do
                    putStrLn "---------------------------------------------"
                    putStrLn "Danke !"

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
         putStrLn "Bitte geben Sie den Ländernamen ein! Auf English "
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

         putStrLn "('CountryName','Bestätigt','Todesfälle','Wieder gesund','Active','Date')"
         putStrLn "------------------------------------------------------------------------"
         mapM_ print countryInfo

         --print (decode countryVirusJson :: Maybe Countries)
         putStrLn "------------------------------------------------------------------------"

         --putStrLn "Filter by Date (von ---> bis)"
         putStrLn "Wollen Sie die Datei in file speichern (y/n)? "
         choiceFile <- getChar
         case choiceFile of
               'y' -> do
                    --print "Bitte geben Sie den Dateinamen ein !, zum Beispiel Deutschland"
                    --fileNameEnter <- getLine
                    let fileName = "data/countries/country.txt"
                        -- // @toDo Fix new File Country
                    writeFile fileName  (show countryInfo)
                    putStrLn "Die Datei wird geschrieben"
               'n' -> do
                    putStrLn "---------------------------------------------"
                    putStrLn "Danke !"

      7 -> do
          putStrLn "Bitte geben Sie den Ländernamen ein! Auf English "
          searchedCountry <- getLine
          let apiUrl = "https://api.covid19api.com/dayone/country/" ++ searchedCountry
          -- // @toDo Von-bis Filer
          putStrLn "Bitte geben Sie das Datum ein ! Y-M-T beispeil 2020-03-04 "
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
          putStrLn "('CountryName','Bestätigt','Todesfälle','Wieder gesund','Active','Date')"
          print searchedByDate

      8 -> do
          putStrLn "Bitte geben Sie den Ländernamen ein! Auf English "
          searchedCountry <- getLine
          let apiUrl = "https://api.covid19api.com/dayone/country/" ++ searchedCountry
          -- // @toDo Von-bis Filer
          putStrLn "Bitte geben Sie das Startdatum ein ! Format: y-m-t beispeil 2020-03-04 "
          startDate <- getLine
          putStrLn "Bitte geben Sie das Enddatum ein ! Format: y-m-t beispeil 2020-03-04"
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

          putStrLn "('CountryName','Bestätigt','Todesfälle','Wieder gesund','Active','Date')"
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

          putStrLn ("Von " ++ startDate ++ " Bis " ++ endDate ++ " Sind " ++ show rangeDays)
          mapM_ print $ rangeCorona countryInfo (indexSearchedElementStart!!0!!0) (indexSearchedElementEnd!!0!!0)
          putStrLn ("---------------------------------------------------------------------------")
          putStrLn ("Am " ++ startDate ++ " Waren " ++ show confirmedStart ++ " Bestätigte Falle !! aber am " ++ endDate ++" Sind " ++ show confirmedEnd ++ " D.h: "  ++ show (fromEnum diffConfiremd) ++ "+" )
          putStrLn ("---------------------------------------------------------------------------")
          putStrLn ("Am " ++ startDate ++ " Waren " ++ show deathStart ++ " Todesfälle  !! aber am " ++ endDate ++" Sind " ++ show deathEnd ++ " D.h: "  ++ show (fromEnum diffDeath) ++ "+" )
          putStrLn ("---------------------------------------------------------------------------")
          putStrLn ("Am " ++ startDate ++ " Waren " ++ show recovredStart ++ " Wieder gesund  !! aber am " ++ endDate ++" Sind " ++ show recovredEnd  ++ " D.h: "  ++ show (fromEnum diffRecovory) ++ "+")
          putStrLn ("---------------------------------------------------------------------------")
          putStrLn ("Am " ++ startDate ++ " Waren " ++ show activeStart ++ " Active  Falle !! aber am " ++ endDate ++" Sind " ++ show activeEnd ++ " D.h: "  ++ show (fromEnum diffActive) ++ "+" )
      otherwise -> do
          putStrLn "Bitte wählen Sie eine Nummer von 1 bis 6"
-- Menu
main :: IO ()
main = do
  menu