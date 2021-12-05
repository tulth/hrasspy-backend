{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ServerApp
    ( startApp
    , app
    ) where

import Servant
    ( serve
      , Application
      , type (:>)
      , JSON
      , Get
      , Proxy
      , Handler
      , type  (:<|>) ((:<|>))
      )
import Network.Wai.Logger ( withStdoutLogger )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Handler.Warp
    ( setLogger, setPort, runSettings, defaultSettings )
import Servant.Server ( Server )
import Data.Proxy (Proxy(Proxy))

import IntentHandlerApi ( IntentApi, intentApiHandler )
import TextToSpeechApi ( TtsApi, ttsApiHandler )

type API = "status" :> Get '[JSON] String
      :<|> IntentApi
      :<|> TtsApi

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

