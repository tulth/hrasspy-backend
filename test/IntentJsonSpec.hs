{-# LANGUAGE OverloadedStrings #-}

module IntentJsonSpec ( testGetTimeIO
                      , testGetTemperatureIO
                      , testChangeLightStateIO
                      ) where

import qualified Data.ByteString.Lazy as BL

import Data.Aeson ( decode )
import Data.Map ( fromList, empty )
import Data.List.Extra ( replace )
import Data.Text ()

import Intent ( Intent(..) )

readIntentFile :: String -> IO (Maybe Intent)
readIntentFile fileName = do
   decode <$> BL.readFile fileName

testIntentNoSlotsIO :: String -> IO Bool
testIntentNoSlotsIO intentN = do
  actual <- readIntentFile fileName
  return $ expect == actual
  where expect = Just (Intent {intentName = intentN, slots = empty})
        fileName = "test/intents/" ++ intentN ++ ".json"

testGetTimeIO :: IO Bool
testGetTimeIO = testIntentNoSlotsIO "GetTime"

testGetTemperatureIO :: IO Bool
testGetTemperatureIO = testIntentNoSlotsIO "GetTemperature"

testIntentWithNameStateIO :: String -> String -> String -> IO Bool
testIntentWithNameStateIO intentN slotN slotS = do
  actual <- readIntentFile fileName
  return $ expect == actual
  where eSlot = fromList [("name", slotN), ("state",slotS)]
        slotN' = replace " " "_" slotN
        expect = Just (Intent {intentName = intentN,
                               slots = eSlot})
        fileBaseName = intentN ++ "-" ++ slotN' ++ "-" ++ slotS
        fileName = "test/intents/" ++ fileBaseName ++ ".json"

testChangeLightStateIO :: IO Bool
testChangeLightStateIO =
  testIntentWithNameStateIO "ChangeLightState" "corner lamp" "on"
