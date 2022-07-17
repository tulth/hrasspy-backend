{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GetWeather ( getWeather
                  , WeatherRecord(..)
                  ) where

import qualified Data.ByteString.Lazy as BL
import HttpRequest ( buildHttpRequest, doHttpRequestBody )
import Data.Aeson (eitherDecode, FromJSON (parseJSON), withObject, (.:))

data WeatherRecord = WeatherRecord
  { weatherCurrentlySummary :: !String
  , weatherCurrentlyTemperature :: !Double
--  , weatherHiTemperature :: !Double
--  , weatherLoTemperature :: !Double
  } deriving (Show)


instance FromJSON WeatherRecord where
  parseJSON = withObject "WeatherRecord" $ \obj -> do
      currently <- obj .: "currently"
      summary <- currently .: "summary"
      temperature <- currently .: "temperature"
      -- daily <- obj .: "daily"
      -- dailyDataList <- daily .: "data"
      -- dailyData0 <-maybeToEither "" listToMaybe dailyDataList
      -- temperatureHi <- dailyData0 .: "temperatureHigh"
      -- temperatureLo <- dailyData0 .: "temperatureLow"
      return $ WeatherRecord { weatherCurrentlySummary = summary
                             , weatherCurrentlyTemperature = temperature
--                             , weatherHiTemperature = temperatureHi
--                             , weatherLoTemperature = temperatureLo
                             }


getWeather :: IO (Either String WeatherRecord)
getWeather = do
  print proxyRequest
  responseBody <- responseBodyIO
  let responseVal = responseBody >>= eitherDecode
  return responseVal
  where proxyRequest = buildHttpRequest "nginx" 8001 "weather/cache-api-response.json" "GET" ""
        responseBodyIO :: IO (Either String BL.ByteString)
        responseBodyIO = doHttpRequestBody proxyRequest
