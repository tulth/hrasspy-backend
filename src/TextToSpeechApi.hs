{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TextToSpeechApi ( TtsApi
                       , ttsApiHandler
                       ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Monad.Except ( liftEither )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except ( withExceptT, ExceptT(..) )
import qualified Network.HTTP.Media as M
import Servant
    ( type (:>),
      Handler,
      err404,
      Handler(Handler),
      Post,
      PlainText,
      ReqBody,
      MimeRender (mimeRender),
      Accept (contentType) )
import Servant.Server ( ServerError )
import HttpRequest ( buildHttpRequest, doHttpRequestBody )
  
type TtsApi = "api" :> "text-to-speech"
  :> ReqBody '[PlainText] TextToConvert
  :> Post '[Wav] WavData

type TextToConvert = String

data Wav

newtype WavData = WavData { gen :: BL.ByteString }

instance Accept Wav where
  contentType _ = "audio" M.// "wav" M./: ("charset", "utf-8")

instance MimeRender Wav WavData where
  mimeRender _ = gen

ttsApiHandler :: TextToConvert -> Handler WavData
ttsApiHandler arg = do
  liftIO $ print proxyRequest
  waveDataE <- liftIO $ doHttpRequestBody proxyRequest
  let waveDataRaw :: ExceptT ServerError IO BL.ByteString
      waveDataRaw = withExceptT (const err404) $ liftEither waveDataE
      waveData :: ExceptT ServerError IO WavData
      waveData = WavData <$> waveDataRaw
  Handler waveData
  where proxyRequest = buildHttpRequest "docker" 12101 "api/text-to-speech" "POST" (BLC.pack arg) 
