{-# LANGUAGE TypeApplications #-}

module HttpRequest ( buildRequest
                   , buildHttpRequest
                   , buildHttpsRequest
                   , doHttpRequestBody
                   ) where

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
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Control.Exception (Exception, try)
  
buildRequest :: Bool          -- ^ use secure protocol, http vs https
             -> String        -- ^ web host name, like google.com
             -> Int           -- ^ the port to use
             -> String        -- ^ url path to access on host
             -> String        -- ^ method, like "POST" "GET"
             -> BL.ByteString -- ^ the request body
             -> Request       -- ^ the resulting request
buildRequest isSecure host port path method body =
  setRequestBodyLBS body
  $ setRequestMethod (BC.pack method)
  $ setRequestHost (BC.pack host)
  $ setRequestPath (BC.pack path)
  $ setRequestSecure isSecure
  $ setRequestPort port
  defaultRequest

buildHttpRequest
  :: String        -- ^ web host name, like google.com
  -> Int           -- ^ the port to use
  -> String        -- ^ url path to access on host
  -> String        -- ^ method, like "POST" "GET"
  -> BL.ByteString -- ^ the request body
  -> Request       -- ^ the resulting request
buildHttpRequest = buildRequest False

buildHttpsRequest
  :: String        -- ^ web host name, like google.com
  -> Int           -- ^ the port to use
  -> String        -- ^ url path to access on host
  -> String        -- ^ method, like "POST" "GET"
  -> BL.ByteString -- ^ the request body
  -> Request       -- ^ the resulting request
buildHttpsRequest = buildRequest True

doHttpRequestBody :: Request -> IO (Either String BL.ByteString)
doHttpRequestBody request = do
  responseE <- exceptionToString <$> (try @HttpException $ httpLBS request)
  let responseBodyE = responseE >>= responseToBody
  return responseBodyE

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
