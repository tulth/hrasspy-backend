module GetSpokenTimeSpec ( testHourMinToStr
                         ) where

import GetSpokenTime

testHourMinToStr :: Bool
testHourMinToStr = and tests
  where
    tests = [ hourMinToStr 0 0 == "midnight"
            , hourMinToStr 0 1 == "zero oh oh 1"
            , hourMinToStr 0 10 == "zero oh 10"
            , hourMinToStr 0 59 == "zero oh 59"
            , hourMinToStr 1 0 == "oh 1 hundred"
            , hourMinToStr 1 2 == "oh 1 oh 2"
            , hourMinToStr 9 0 == "oh 9 hundred"
            , hourMinToStr 9 2 == "oh 9 oh 2"
            , hourMinToStr 10 2 == "10 oh 2"
            , hourMinToStr 23 59 == "23 59"
            ]

