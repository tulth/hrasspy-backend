{-# LANGUAGE OverloadedStrings #-}

module Intent
    ( Intent(..)
    ) where

import Data.Aeson
    ( (.:)
    , withObject
    , FromJSON(parseJSON) )
import Data.Text ()
import Data.Map ( Map )

data Intent = Intent
  { intentName :: String
  , slots :: Map String String
  } deriving ( Show, Eq )

instance FromJSON Intent where
    parseJSON = withObject "Intent" $ \obj -> do
        int <- obj .: "intent"
        intNom <- int .: "name"
        slotsVal <- obj .: "slots"
        return $ Intent intNom slotsVal
