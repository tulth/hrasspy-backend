{-# LANGUAGE OverloadedStrings #-}

module IntentJsonSpec ( testGetTimeIO
                      , testGetTemperatureIO
                      , testChangeLightStateIO
                      ) where

import Data.Map ()
import Data.Text ()
import Data.List.Extra ( replace )
import qualified Data.ByteString.Lazy as BL

import Intent ( getIntent, Intent(..), Slot(..) )
import Data.Aeson (Object, decode)

readIntentFileNoSlots :: String -> IO (Maybe Intent)
readIntentFileNoSlots intentN = do
   getTime <- BL.readFile fileName
   let objM = decode getTime :: Maybe Object
   return $ getIntent =<< objM
   where fileName = "test/intents/" ++ intentN ++ ".json"

testIntentNoSlotsIO :: String -> IO Bool
testIntentNoSlotsIO intentN = do
  actual <- readIntentFileNoSlots intentN
  return $ expect == actual
  where expect = Just (Intent {intentName = intentN, slots = []})
  
testGetTimeIO :: IO Bool
testGetTimeIO = testIntentNoSlotsIO "GetTime"

testGetTemperatureIO :: IO Bool
testGetTemperatureIO = testIntentNoSlotsIO "GetTemperature"
  
readIntentFileWithOneSlot :: String -> String -> String -> IO (Maybe Intent)
readIntentFileWithOneSlot intentN slotN slotS = do
   getTime <- BL.readFile fileName
   let objM = decode getTime :: Maybe Object
   return $ getIntent =<< objM
   where slotN' = replace " " "_" slotN
         fileBaseName = intentN ++ "-" ++ slotN' ++ "-" ++ slotS
         fileName = "test/intents/" ++ fileBaseName ++ ".json"

testIntentWithOneSlotIO :: String -> String -> String -> IO Bool
testIntentWithOneSlotIO intentN slotN slotS = do
  actual <- readIntentFileWithOneSlot intentN slotN slotS
  return $ expect == actual
  where eSlot = Slot {slotName=slotN, slotState=slotS}
        expect = Just (Intent {intentName = intentN,
                               slots = [eSlot]})

testChangeLightStateIO :: IO Bool                            
testChangeLightStateIO =
  testIntentWithOneSlotIO "ChangeLightState" "corner lamp" "on"
