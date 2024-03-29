{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntentHandlerApi ( IntentApi
                        , intentApiHandler
                        ) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Monad.IO.Class (MonadIO(liftIO))
import Servant
    ( type (:>)
    , Handler
    , JSON
    , Post
    , ReqBody
    )

import Data.Aeson ( object, ToJSON(..), (.=), )
import Data.Char ( isUpper, toUpper )
import Data.List.Extra ( replace, lower )
import qualified Data.Map as Map

import Intent ( Intent(..) )
import HttpRequest ( buildHttpRequest, doHttpRequestBody )
import GetSpokenTime ( getSpokenTimeIO )
import GetWeather ( getWeather
                  , WeatherRecord(..) )
import Network.HTTP.Simple ( Request )

type IntentApi = "api" :> "intent"
  :> ReqBody '[JSON] Intent
  :> Post '[JSON] ResponseToSpeak

newtype ResponseToSpeak = ResponseToSpeak String deriving ( Show )

instance ToJSON ResponseToSpeak where
  toJSON (ResponseToSpeak a) = object ["speech" .= object [ "text" .= toJSON a] ]

intentApiHandler :: Intent -> Handler ResponseToSpeak
intentApiHandler argIntent = do
  liftIO $ print argIntent
  case argIntentName of
    "GetTime" -> doGetTime argIntent
    "GetTemperature" -> doGetWeather argIntent
    "ChangeLightState" -> changeLightState argIntent
    "ChangeHomeTheaterState" -> changeHomeTheaterState argIntent
    "ChangeHomeTheaterVolume" -> changeHomeTheaterVolume argIntent
    _ -> unhandled
  where argIntentName = intentName argIntent
        unhandled = liftIO $ return $ ResponseToSpeak $
          "I don't handle " ++ getSpokenName argIntentName

doGetTime :: Intent -> Handler ResponseToSpeak
doGetTime = do
  let strReplyIO = ("The time is " ++) <$> getSpokenTimeIO
  return $ liftIO $ ResponseToSpeak <$> strReplyIO

changeLightState :: Intent -> Handler ResponseToSpeak
changeLightState argIntent =
  case (sNameM, sStateM) of
    (Just sName, Just sState) -> liftIO $ ResponseToSpeak <$>
      changeLightState' sName sState
    _ -> err
  where argIntentName = intentName argIntent
        argSlots = slots argIntent
        sNameM = "name" `Map.lookup` argSlots
        sStateM = "state" `Map.lookup` argSlots
        err = liftIO $ return $ ResponseToSpeak $
                "Error handling " ++ getSpokenName argIntentName

changeLightState' :: String -> String -> IO String
changeLightState' sName sState =
  case itemNameM of
    Nothing -> return $ "I did not understand switch " ++ sName
    Just inom -> do
      success <- openhabHttpActionIO inom itemState
      if success
        then return $ unwords ["setting", sName, sState]
        else return $ unwords ["Error setting", sName, sState]
  where itemNameM = lightNameToItemName sName
        itemState = lightStateToItemState $ map toUpper sState

lightNameToItemName :: String -> Maybe String
lightNameToItemName "corner lamp"         = Just "corner_lamp"
lightNameToItemName "den lamp"            = Just "corner_lamp"
lightNameToItemName "den"                 = Just "corner_lamp"
lightNameToItemName "curio cabinet"       = Just "curio_cabinet_switch_dimmer"
lightNameToItemName "cabinet"             = Just "curio_cabinet_switch_dimmer"
lightNameToItemName "master bedroom lamp" = Just "master_bedroom_lamp"
lightNameToItemName "bedroom lamp"        = Just "master_bedroom_lamp"
lightNameToItemName _                     = Nothing

lightStateToItemState :: String -> String
lightStateToItemState "0"          = "OFF"
lightStateToItemState "1"          = "ON"
lightStateToItemState "ACTIVE"     = "ON"
lightStateToItemState "ACTIVATE"   = "ON"
lightStateToItemState "INACTIVE"   = "OFF"
lightStateToItemState "DEACTIVATE" = "OFF"
lightStateToItemState other        = other

changeHomeTheaterState :: Intent -> Handler ResponseToSpeak
changeHomeTheaterState argIntent =
  case sNameM of
    Just sName -> liftIO $ ResponseToSpeak <$>
      changeHomeTheaterState' sName
    _ -> err
  where argIntentName = intentName argIntent
        argSlots = slots argIntent
        sNameM = "name" `Map.lookup` argSlots
        err = liftIO $ return $ ResponseToSpeak $
                "Error handling " ++ getSpokenName argIntentName

changeHomeTheaterState' :: String -> IO String
changeHomeTheaterState' argName =
  case itemNameM of
    Nothing -> return $ "I did not understand activity " ++ argName
    Just inom -> do
      success <- openhabHttpActionIO inom "ON"
      if success
        then return $ unwords ["doing activity", getSpokenName' inom]
        else return $ unwords ["Error setting", getSpokenName' inom]
  where itemNameM = homeTheaterActivityToItemName argName
        getSpokenName' = getSpokenName . replace "tv_" ""

getSpokenName :: String -> String
getSpokenName = replace "_" " " . camelCaseToSpaced

homeTheaterActivityToItemName :: String -> Maybe String
homeTheaterActivityToItemName x
  | x `elem` ["off", "shutdown", "shut down"]                                  = Just "tv_shutdown"
  | x `elem` ["bed time", "bedtime", "timer", "time"]                          = Just "tv_bedtime"
  | x `elem` ["youtube", "you tube", "you too", "u2"]                          = Just "tv_youtube"
  | x `elem` ["curiosity stream", "curiosity steam"]                           = Just "tv_curiositystream"
  | x `elem` ["amazon prime", "prime"]                                         = Just "tv_amazon_prime"
  | x == "twitch"                                                              = Just "tv_twitch"
  | x `elem` ["my movies", "jellyfin", "jelly fin", "jellyfish", "jelly fish"] = Just "tv_jellyfin"
  | x == "kodi"                                                                = Just "tv_kodi"
  | x `elem` ["flex", "netflix"]                                               = Just "tv_netflix"
  | x `elem` ["apple tv", "apply tv", "apple t. v.", "apple", "apply t. v."]   = Just "tv_apple_tv"
  | x `elem` ["disney", "disney plus"]                                         = Just "tv_disney"
  | otherwise                                                                  = Nothing
-- FIXME? | x `elem` ["dex", "samsung dex"]                   = "tv_"
-- FIXME? | x `elem` ["game", "steam", "gaming", "steen", "seen", "gaining", "steam link"]        = "tv_steam"
-- FIXME?  | x == "test"                                       = Nothing

openhabHttpActionIO :: String -> String -> IO Bool
openhabHttpActionIO  =
  openhabHttpRequestIO `blackbird` openhabItemPostRequest

blackbird :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
blackbird = (.) . (.)

openhabHttpRequestIO :: Request -> IO Bool
openhabHttpRequestIO req = do
  print req
  httpActionE <- httpActionIOE
  print httpActionE
  either (const False)
    (const True)
    <$> httpActionIOE
  where httpActionIOE = doHttpRequestBody req

openhabItemPostRequest :: String -> String -> Request
openhabItemPostRequest itemName itemState =
  buildHttpRequest "burpelson" 8080 ("rest/items/" ++ itemName)  "POST" (BLC.pack itemState)

camelCaseToSpaced :: String -> String
camelCaseToSpaced "" = ""
camelCaseToSpaced (a:as) =
  if isUpper a
  then reverse ( a : " ") ++ camelCaseToSpaced as
  else a : camelCaseToSpaced as

doGetWeather :: Intent -> Handler ResponseToSpeak
doGetWeather _ =
  liftIO resp
  where resp = ResponseToSpeak . doGetWeather' <$> getWeather

doGetWeather' :: Either String WeatherRecord -> String
doGetWeather' arg =
  case arg of
    Right w -> goodReply w
    Left _ -> "Error getting temperature"
  where goodReply :: WeatherRecord -> String
        goodReply argW = unwords [
          "The current weather is"
          , show(round(weatherCurrentlyTemperature argW) :: Integer)
          , "degrees and"
          , weatherCurrentlySummary argW]


changeHomeTheaterVolume :: Intent -> Handler ResponseToSpeak
changeHomeTheaterVolume argIntent =
  case sStateM of
    Just sState -> liftIO $ ResponseToSpeak <$>
      changeHomeTheaterVolume' sState
    _ -> err
  where argIntentName = intentName argIntent
        argSlots = slots argIntent
        sStateM = "state" `Map.lookup` argSlots
        err = liftIO $ return $ ResponseToSpeak $
                "Error handling " ++ getSpokenName argIntentName

changeHomeTheaterVolume' :: String -> IO String
changeHomeTheaterVolume' argVolume =
  case requestsM of
    Nothing -> return $ "I did not understand volume " ++ argVolume
    Just reqs -> do
      success <- and <$> sequence (openhabHttpRequestIO <$> reqs)
      if success
        then return $ unwords ["setting volume", argVolume]
        else return $ unwords ["Error setting volume", argVolume]
  where argVolumeLower = lower argVolume
        requestsM = volumeStateToRequests argVolumeLower

volumeStateToRequests :: String -> Maybe [Request]
volumeStateToRequests = fmap volumeUpdateToRequests . volumeStateToVolumeUpdate

data VolumeUpdate = VolumeMute
                  | VolumeUnmute
                  | VolumeAbsolute Int
                  | VolumeIncrease
                  | VolumeDecrease
    deriving ( Show )

volumeUpdateToRequests :: VolumeUpdate -> [Request]
volumeUpdateToRequests (VolumeAbsolute n) = [ openhabItemPostRequest "yamaha_volume" (show n)]
volumeUpdateToRequests VolumeIncrease     = [ openhabItemPostRequest "yamaha_volume" "INCREASE"
                                            -- , openhabItemPostRequest "yamaha_volume" "INCREASE"
                                            ]
volumeUpdateToRequests VolumeDecrease     = [ openhabItemPostRequest "yamaha_volume" "DECREASE"
                                            , openhabItemPostRequest "yamaha_volume" "DECREASE"
                                            ]
volumeUpdateToRequests VolumeMute         = [ openhabItemPostRequest "yamaha_mute" "ON"]
volumeUpdateToRequests VolumeUnmute       = [ openhabItemPostRequest "yamaha_mute" "OFF"]

volumeStateToVolumeUpdate :: String -> Maybe VolumeUpdate
volumeStateToVolumeUpdate "high"    = Just (VolumeAbsolute 65)
volumeStateToVolumeUpdate "medium"  = Just (VolumeAbsolute 56)
volumeStateToVolumeUpdate "low"     = Just (VolumeAbsolute 47)
volumeStateToVolumeUpdate "up"      = Just VolumeIncrease
volumeStateToVolumeUpdate "raise"   = Just VolumeIncrease
volumeStateToVolumeUpdate "louder"  = Just VolumeIncrease
volumeStateToVolumeUpdate "quieter" = Just VolumeDecrease
volumeStateToVolumeUpdate "down"    = Just VolumeDecrease
volumeStateToVolumeUpdate "lower"   = Just VolumeDecrease
volumeStateToVolumeUpdate "mute"    = Just VolumeMute
volumeStateToVolumeUpdate "unmute"  = Just VolumeUnmute
volumeStateToVolumeUpdate _         = Nothing

