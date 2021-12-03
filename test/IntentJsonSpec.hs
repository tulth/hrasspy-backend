{-# LANGUAGE OverloadedStrings #-}

module IntentJsonSpec ( test
                      , test2
                      ) where

import Data.Map ( Map )
import Data.Aeson ( decode, (.:) )
import Data.Aeson.Types ( Parser, parseMaybe )

test :: Maybe (Map String Int)
test = decode "{\"foo\":1,\"bar\":2}"

test2 :: Maybe String
test2 = do
  result <- decode "{\"name\":\"Dave\",\"age\":2}"
  flip parseMaybe result $ \obj -> do
    let ageM :: Parser Integer
        ageM = obj .: "age"
    age <- ageM
    name <- obj .: "name"
    return (name ++ ": " ++ show (age*2))
