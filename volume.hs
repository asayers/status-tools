import System.Process (readProcess)
import Control.Monad (liftM)

data MuteState = Muted | Unmuted

parseVolume :: String -> Int
parseVolume str = read . takeWhile (/= '%') . drop 1 $ words (lines str !! 5) !! 4

parseMute :: String -> MuteState
parseMute str = if state == "on"
                  then Muted
                  else Unmuted
  where state = words (lines str !! 1) !! 1

output :: Int -> MuteState -> String
output vol mute = concat [ "<fc=", colourise mute, ">"
                         , show vol
                         , "</fc>%"
                         ]
  where colourise Muted   = "red"
        colourise Unmuted = "green"

main :: IO ()
main = do
  vol <- liftM parseVolume $ readProcess "amixer" ["get", "Master"] ""
  mute <- liftM parseMute $ readFile "/proc/acpi/ibm/volume"
  putStrLn $ output vol mute
