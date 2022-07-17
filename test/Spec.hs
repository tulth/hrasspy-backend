import GetSpokenTimeSpec ( testHourMinToStr )
import IntentJsonSpec ( testGetTimeIO
                      , testGetTemperatureIO
                      , testChangeLightStateIO
                      )


assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

main :: IO ()
main = do
  putStrLn "Running tests..."
  assert testHourMinToStr "passed 'testHourMinToStr'" "failed 'testHourMinToStr'"
  getTimePassed <- testGetTimeIO
  assert getTimePassed "passed 'GetTime'" "FAIL: 'GetTime'"
  getTemperaturePassed <- testGetTemperatureIO
  assert getTemperaturePassed "passed 'GetTemperature'" "FAIL: 'GetTemperature'"
  testChangeLightStatePassed <- testChangeLightStateIO
  assert testChangeLightStatePassed "passed 'ChangeLightState'" "FAIL: 'ChangeLightState'"
  putStrLn "done!"

