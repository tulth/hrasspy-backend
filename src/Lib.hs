{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( startApp
    , app
    ) where

import Servant
    ( serve,
      Application,
      type (:>),
      JSON,
      Get,
      Handler,
      Proxy,
      err404,
      Handler(Handler),type  (:<|>) ((:<|>)), Post, PlainText, ReqBody, MimeRender (mimeRender), Accept (contentType) )
import Network.Wai.Logger ( withStdoutLogger )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Handler.Warp
    ( setLogger, setPort, runSettings, defaultSettings )
import Servant.Server (Server, ServerError)
import Data.Proxy (Proxy(Proxy))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Except ( withExceptT, ExceptT(..) )
import Network.HTTP.Simple
  ( Request
  , defaultRequest
  , setRequestHost
  , setRequestPort
  , setRequestPath
  , setRequestMethod
  , setRequestSecure
  , setRequestBodyLBS
  , Response
  , getResponseStatusCode
  , getResponseBody
  , httpLBS
  , HttpException(..)
  )
import Control.Exception (Exception, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Except ( liftEither )
import qualified Network.HTTP.Media as M
import Data.Aeson ( object, ToJSON(..), (.=), )
import Data.Aeson.Types ( Object )

import Intent ( getIntent, Intent(..), Slot (..) )
import GetSpokenTime ( getSpokenTimeIO )
import Data.Char ( isUpper, toUpper )
import Data.Maybe (listToMaybe)

type API = "status" :> Get '[JSON] String
      :<|> "api" :> "intent" :> ReqBody '[JSON] IntentJsonObj :> Post '[JSON] ResponseToSpeak
      :<|> "api" :> "text-to-speech" :> ReqBody '[PlainText] TextToConvert :> Post '[Wav] WavData

startApp :: IO ()
startApp = do
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings app

app :: Application
app = logStdoutDev $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = statusHandler
    :<|> intentApiHandler
    :<|> ttsApiHandler
  where statusHandler :: Handler String
        statusHandler = return "up"

ttsApiHandler :: TextToConvert -> Handler WavData
ttsApiHandler arg = do
  liftIO $ print proxyRequest
  waveDataE <- liftIO $ doHttpRequestBody proxyRequest

  let waveDataRaw :: ExceptT ServerError IO BL.ByteString
      waveDataRaw = withExceptT (const err404) $ liftEither waveDataE
      waveData :: ExceptT ServerError IO WavData
      waveData = WavData <$> waveDataRaw
  Handler waveData
  where proxyRequest = setRequestBodyLBS (BLC.pack arg) $ buildRequest "docker" "POST" "api/text-to-speech" False 12101

type TextToConvert = String

data Wav

newtype WavData = WavData { gen :: BL.ByteString }

instance Accept Wav where
  contentType _ = "audio" M.// "wav" M./: ("charset", "utf-8")

instance MimeRender Wav WavData where
  mimeRender _ = gen

exceptionToString :: Exception e => Either e a -> Either String a
exceptionToString = either (Left . show) Right

responseToBody :: Response BL.ByteString -> Either String BL.ByteString
responseToBody response =
  if status == 200
  then Right body
  else Left errMsg
  where status = getResponseStatusCode response
        body = getResponseBody response
        errMsg = "request failed with bad status " ++ show status

doHttpRequestBody :: Request -> IO (Either String BL.ByteString)
doHttpRequestBody request = do
  responseE <- exceptionToString <$> (try @HttpException $ httpLBS request)
  let responseBodyE = responseE >>= responseToBody
  return responseBodyE

buildRequest :: String -> String
             -> String -> Bool -> Int -> Request
buildRequest host method path isSecure port =
  setRequestMethod (BC.pack method)
  $ setRequestHost (BC.pack host)
  $ setRequestPath (BC.pack path)
  $ setRequestSecure isSecure
  $ setRequestPort port
  defaultRequest

type IntentJsonObj = Object
newtype ResponseToSpeak = ResponseToSpeak String

instance ToJSON ResponseToSpeak where
  toJSON (ResponseToSpeak a) = object ["speech" .= object [ "text" .= toJSON a] ]

intentApiHandler :: IntentJsonObj -> Handler ResponseToSpeak
intentApiHandler jsonBody = do
  let intentM = getIntent jsonBody
      intentNameM = intentName <$> intentM
      slotM = getSingleSlot intentM
  response <- liftIO $
    case intentNameM of
      Just "GetTime" -> ("The time is " ++) <$> getSpokenTimeIO
      Just "ChangeLightState" -> changeLightStateM slotM
      Just inom -> return $ "I don't handle " ++ camelCaseToSpaced inom
      Nothing -> return "I did not understand"
  return $ ResponseToSpeak response

camelCaseToSpaced :: String -> String
camelCaseToSpaced "" = ""
camelCaseToSpaced (a:as) =
  if isUpper a
  then reverse ( a : " ") ++ camelCaseToSpaced as
  else a : camelCaseToSpaced as

getSingleSlot :: Maybe Intent -> Maybe Slot
getSingleSlot argIntentM = do
  argIntent <- argIntentM
  listToMaybe $ slots argIntent

changeLightStateM :: Maybe Slot -> IO String
changeLightStateM argSlotM = case argSlotM of
  Nothing -> return "missing slot"
  Just a -> changeLightState a

changeLightState :: Slot -> IO String
changeLightState Slot {slotName=sName, slotState=sState} =
  case itemNameM of
    Nothing -> return $ "I did not understand switch " ++ sName
    Just inom -> openhabHttpActionIO inom itemState
  where itemNameM = slotNameToItemName sName
        itemState = map toUpper sState

slotNameToItemName :: String -> Maybe String
slotNameToItemName "corner lamp" = Just "corner_lamp"
slotNameToItemName "den lamp" = Just "corner_lamp"
slotNameToItemName "den" = Just "corner_lamp"
slotNameToItemName "curio cabinet" = Just "curio_cabinet_switch_dimmer"
slotNameToItemName "cabinet" = Just "curio_cabinet_switch_dimmer"
slotNameToItemName _ = Nothing

openhabHttpActionIO :: String -> String -> IO String
openhabHttpActionIO itemName itemState = do
  either (const "error")
    (const $ "setting " ++ itemName ++ " to " ++ itemState)
    <$> httpActionIOE
  where req = setRequestBodyLBS (BLC.pack itemState) $
          buildRequest "burpelson" "POST" ("rest/items/" ++ itemName) False 8080
        httpActionIOE = doHttpRequestBody req
