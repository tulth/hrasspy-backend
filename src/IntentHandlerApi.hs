{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

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
import HttpRequest ( buildHttpRequest, doHttpRequestBody )
import Data.Aeson ( object, ToJSON(..), (.=), )

import Intent ( Intent(..) )
import GetSpokenTime ( getSpokenTimeIO )
import Data.Char ( isUpper, toUpper )
import Data.List.Extra ( replace )
import qualified Data.Map as Map

type IntentApi = "api" :> "intent"
  :> ReqBody '[JSON] Intent
  :> Post '[JSON] ResponseToSpeak

newtype ResponseToSpeak = ResponseToSpeak String

instance ToJSON ResponseToSpeak where
  toJSON (ResponseToSpeak a) = object ["speech" .= object [ "text" .= toJSON a] ]

intentApiHandler :: Intent -> Handler ResponseToSpeak
intentApiHandler argIntent = do
  liftIO $ print $ argIntent
  case argIntentName of
    "GetTime" -> doGetTime argIntent
    "ChangeLightState" -> changeLightState argIntent
    "ChangeHomeTheaterState" -> changeHomeTheaterState argIntent
    _ -> unhandled
  where argIntentName = intentName argIntent
        unhandled = liftIO $ return $ ResponseToSpeak $
          "I don't handle " ++ camelCaseToSpaced argIntentName

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
                "Error handling " ++ camelCaseToSpaced argIntentName
          
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
lightNameToItemName "corner lamp"   = Just "corner_lamp"
lightNameToItemName "den lamp"      = Just "corner_lamp"
lightNameToItemName "den"           = Just "corner_lamp"
lightNameToItemName "curio cabinet" = Just "curio_cabinet_switch_dimmer"
lightNameToItemName "cabinet"       = Just "curio_cabinet_switch_dimmer"
lightNameToItemName _               = Nothing

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
                "Error handling " ++ camelCaseToSpaced argIntentName

changeHomeTheaterState' :: String -> IO String
changeHomeTheaterState' argName =
  case itemNameM of
    Nothing -> return $ "I did not understand activity " ++ argName
    Just inom -> do
      success <- openhabHttpActionIO inom "ON"
      if success
        then return $ unwords ["doing activity", getSpokenName inom]
        else return $ unwords ["Error setting", getSpokenName inom]
  where itemNameM = homeTheaterActivityToItemName argName
        getSpokenName :: String -> String
        getSpokenName = replace "tv_" ""

homeTheaterActivityToItemName :: String -> Maybe String  
homeTheaterActivityToItemName x
  | x `elem` ["off", "shutdown", "shut down"]                                  = Just "tv_shutdown"
  | x `elem` ["bed time", "bedtime", "timer", "time"]                          = Just "tv_bedtime"
  | x `elem` ["youtube", "you tube", "you too", "u2"]                          = Just "tv_youtube"
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
openhabHttpActionIO itemName itemState = do
  print req
  either (const False)
    (const True)
    <$> httpActionIOE
  where req = buildHttpRequest "burpelson" 8080 ("rest/items/" ++ itemName)  "POST" (BLC.pack itemState)
        httpActionIOE = doHttpRequestBody req

camelCaseToSpaced :: String -> String
camelCaseToSpaced "" = ""
camelCaseToSpaced (a:as) =
  if isUpper a
  then reverse ( a : " ") ++ camelCaseToSpaced as
  else a : camelCaseToSpaced as

