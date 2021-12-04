{-# LANGUAGE OverloadedStrings #-}

module IntentJsonSpec ( test1
                      , test2
                      ) where

import Data.Map ( Map )
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL

-- test0 :: IO (Maybe String)
-- test0 = do
--   getTime <- BL.readFile "/home/tulth/local/hrasspy-backend/test/intents/get-time.json"
--   let x = flip parseMaybe getTime $ (\obj -> obj .: "text")
--     -- fullText <- 
--     -- return fullText
--     -- let textKey = pack "text"
--     --     fullTextM :: Parser String
--     --     fullTextM = obj .: textKey
--   return x

test1 :: Maybe (Map String Int)
test1 = decode "{\"foo\":1,\"bar\":2}"

test2 :: Maybe String
test2 = do
  result <- decode "{\"name\":\"Dave\",\"age\":2}"
  flip parseMaybe result $ \obj -> do
    let ageM :: Parser Integer
        ageM = obj .: "age"
    age <- ageM
    name <- obj .: "name"
    return (name ++ ": " ++ show (age*2))

getIntentName :: Object -> Maybe String
getIntentName decodedJsonM = 
  flip parseMaybe decodedJsonM $ \obj -> do
    intent <- obj .: "intent"
    intent .: "name"

-- test3 :: BL.ByteString -> Maybe String
-- test3 content = getIntentName $ decode content

test3 :: BL.ByteString -> Maybe String
test3 content = do
  decodedJsonM <- decode content
  getIntentName decodedJsonM
  -- flip parseMaybe decodedJsonM $ \obj -> do
  --   intent <- obj .: "intent"
  --   intent .: "name"

test4 :: IO (Maybe String)
test4 = do
   getTime <- BL.readFile "/home/tulth/local/hrasspy-backend/test/intents/get-time.json"
   return $ test3 getTime
