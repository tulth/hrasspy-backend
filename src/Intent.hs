{-# LANGUAGE OverloadedStrings #-}

module Intent
    ( getIntent
    , Intent(..)
    , Slot(..)
    ) where

import Data.Aeson ( Object, (.:) )
import Data.Aeson.Types ( parseMaybe )
import Data.Text ()
import Data.Maybe (maybeToList)

data Intent = Intent
  { intentName :: !String
  , slots :: [Slot]
  } deriving ( Show, Eq )

data Slot = Slot
  { slotName :: !String
  , slotState :: !String
  } deriving ( Show, Eq )

getIntent :: Object -> Maybe Intent
getIntent decodedJson =
  (`Intent` slts) <$> inom
  where inom = getIntentName decodedJson
        slts = getIntentSlot decodedJson
        
getIntentName :: Object -> Maybe String
getIntentName decodedJson =
  flip parseMaybe decodedJson $ \obj -> do
    intent <- obj .: "intent"
    intent .: "name"

getIntentSlot :: Object -> [Slot]
getIntentSlot decodedJson =
  maybeToList slotMaybe
  where slotMaybe = flip parseMaybe decodedJson $ \obj -> do
          slts <- obj .: "slots"
          slotN <- slts .: "name"
          slotS <- slts .: "state"
          return Slot {slotName=slotN, slotState=slotS}

