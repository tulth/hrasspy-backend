module GetSpokenTime ( getSpokenTimeIO
                     , hourMinToStr
                     ) where

import Data.Time ( getZonedTime
                 , LocalTime (localTimeOfDay)
                 , TimeOfDay (todHour, todMin), ZonedTime (zonedTimeToLocalTime) )

getSpokenTimeIO :: IO String
getSpokenTimeIO = do
  now <- getZonedTime
  let tod = localTimeOfDay $ zonedTimeToLocalTime now
      nowHour = todHour tod
      nowMin = todMin tod
  return $ hourMinToStr nowHour nowMin

hourMinToStr :: Int -> Int -> String
hourMinToStr 0 0 = "midnight"
hourMinToStr argHour argMin = 
  unwords [hourToStr argHour, minuteToStr argMin]

    
hourToStr :: Int -> String
hourToStr arg 
  | arg == 0 = "zero oh"
  | arg < 10 = "oh " ++ show arg
  | otherwise = show arg

minuteToStr :: Int -> String
minuteToStr arg 
  | arg == 0 = "hundred"
  | arg < 10 = "oh " ++ show arg
  | otherwise = show arg

