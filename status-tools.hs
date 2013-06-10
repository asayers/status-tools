import System.Process (readProcess)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import System.Timeout (timeout)

type BatState = (Int, Power)
data Power = Charging | Discharging

data MuteState = Muted | Unmuted

data NetState = Connected | Connecting | Disconnected | Unavailable | Other String
  deriving (Show)

---- Battery

-- Parse the output from `acpi`
parsePercent :: String -> Int
parsePercent str = read . takeWhile (/= '%') $ str

parsePower :: String -> Power
parsePower str = if str == "Charging,"
                   then Charging
                   else Discharging
                  
parse :: String -> BatState
parse str = (percent, power)
  where percent = parsePercent $ fields !! 3
        power   = parsePower   $ fields !! 2
        fields  = words str

-- Generate output

stateColour :: BatState -> String
stateColour (_, Charging) = "yellow"
stateColour (x, Discharging)
  | x < 10    = "red"
  | otherwise = "green"

batOutput :: BatState -> String
batOutput state =
  concat [ "<fc=", colour, ">"
         , percent
         , "</fc>%"
         ]
  where percent = show . fst $ state
        colour  = stateColour state

---- Volume

parseVolume :: String -> Int
parseVolume str = read . takeWhile (/= '%') . drop 1 $ words (lines str !! 5) !! 4

parseMute :: String -> MuteState
parseMute str = if state == "on"
                  then Muted
                  else Unmuted
  where state = words (lines str !! 1) !! 1

volOutput :: Int -> MuteState -> String
volOutput vol mute = concat [ "<fc=", colourise mute, ">"
                         , show vol
                         , "</fc>%"
                         ]
  where colourise Muted   = "red"
        colourise Unmuted = "green"

---- Network
  
readState :: String -> NetState
readState "connected"    = Connected
readState "disconnected" = Disconnected
readState "unavailable"  = Unavailable
readState str = if "connecting" `isPrefixOf` str
                  then Connecting
                  else Other str

-- Returns [wifi, eth]
parseDevices :: String -> [NetState]
parseDevices str = map (readState . extract) [1,2]
  where extract n = words (lines str !! n) !! 2

netOutput :: [NetState] -> String -> String
netOutput [_, Connected] ip = "<fc=yellow>"++ip++"</fc>"
netOutput [Connected, _] ip = "<fc=green>"++ip++"</fc>"
netOutput [Disconnected, _] _ = "<fc=darkred>disconnected</fc>"
netOutput [Unavailable, _] _ = "<fc=darkred>unavailable</fc>"
netOutput [_, Connecting] _ = "<fc=yellow>connecting...</fc>"
netOutput [Connecting, _] _ = "<fc=orange>connecting...</fc>"
netOutput [Other x, _] _ = "<fc=blue>"++x++"</fc>"

safeIP :: IO String
safeIP = do
   ip <- timeout 10000 $ readProcess "hostname" ["-i"] ""
   return . head . words $ fromMaybe "127.0.0.1" ip

---- Main

main :: IO ()
main = do
  putStr "Bat: "
  readProcess "acpi" ["-b"] "" >>= putStr . batOutput . parse
  putStr " | Vol: "
  vol <- liftM parseVolume $ readProcess "amixer" ["get", "Master"] ""
  mute <- liftM parseMute $ readFile "/proc/acpi/ibm/volume"
  putStr $ volOutput vol mute
  putStr " | Net: "
  devs <- liftM parseDevices $ readProcess "nmcli" ["dev"] ""
  ip   <- safeIP
  putStr $ netOutput devs ip
