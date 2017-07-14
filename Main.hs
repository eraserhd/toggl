{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Time
import GHC.Generics
import Network.HTTP.Simple
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as B

centralParkProject :: Integer
centralParkProject = 38030885

data TimeEntry =
  TimeEntry { description :: String
            , tags :: [String]
            , duration :: Integer
            , start :: UTCTime
            , pid :: Integer
            , created_with :: String
            }
  deriving (Generic, Show)

data CreateTimeEntry =
  CreateTimeEntry { time_entry :: TimeEntry
                  }
  deriving (Generic, Show)

instance ToJSON TimeEntry where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON CreateTimeEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TimeEntry
instance FromJSON CreateTimeEntry

hourToday :: TimeZone -> UTCTime -> Int -> UTCTime
hourToday tz now hour =
  let today = localDay (utcToLocalTime tz now) in
  localTimeToUTC tz (LocalTime today (TimeOfDay 9 0 0))

timeEntryForToday :: TimeZone -> UTCTime -> TimeEntry
timeEntryForToday tz now =
  TimeEntry { description = ""
            , tags = []
            , duration = 8 * 60 * 60
            , start = hourToday tz now 9
            , pid = centralParkProject
            , created_with = "curl"
            }

makeRequest :: CreateTimeEntry -> String -> IO Request
makeRequest body token = do
  req <- parseRequest "https://www.toggl.com/api/v8/time_entries"
  return $
    setRequestMethod "POST" $
    addRequestHeader "Content-Type" "application/json" $
    setRequestBasicAuth (B.pack token) "api_token" $
    setRequestBodyJSON body $
    req


main :: IO ()
main = do now <- getCurrentTime
          tz <- getTimeZone now
          token <- getEnv "TOGGL_API_TOKEN"
          request <- makeRequest (CreateTimeEntry { time_entry = timeEntryForToday tz now }) token
          response <- httpNoBody request
          putStrLn $ show $ getResponseStatus response
