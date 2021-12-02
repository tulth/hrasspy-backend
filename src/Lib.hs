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
import Network.Wai.Handler.Warp
    ( setLogger, setPort, runSettings, defaultSettings )
import Servant.Server (Server, ServerError)
import Data.Proxy (Proxy(Proxy))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Except ( withExceptT, runExceptT, ExceptT(..) )
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

type API = "status" :> Get '[JSON] String
      :<|> "api" :> "text-to-speech" :> ReqBody '[PlainText] TextToConvert :> Post '[Wav] WavData

startApp :: IO ()
startApp = do
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = statusHandler
    :<|> ttsApiHandler
  where statusHandler :: Handler String
        statusHandler = return "up\n"

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
