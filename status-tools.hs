import System.Process (readProcess)
import Control.Monad (liftM, liftM2)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import System.Timeout (timeout)

type BatState = (Int, Bool)

data NetState = Connected | Connecting | Disconnected | Unavailable | Other String
  deriving (Show)

---- Battery

parsePercent :: String -> Int
parsePercent str = read . takeWhile (/= '%') $ str

-- returns True iff charging
parsePower :: String -> Bool
parsePower str = str == "Charging,"

parseBat :: String -> BatState
parseBat str = (percent, power)
  where percent = parsePercent $ fields !! 3
        power   = parsePower   $ fields !! 2
        fields  = words str

batOutput :: BatState -> String
batOutput state = concat [ "<fc=", colour state, ">"
                         , percent
                         , "</fc>%"
                         ]
  where percent          = show . fst $ state
        colour (_, True) = "yellow"
        colour (x, False)
          | x < 10       = "red"
          | otherwise    = "green"


---- Volume

parseVolume :: String -> Int
parseVolume str = read . takeWhile (/= '%') . drop 1 $ extractWord str 5 4

-- returns True if muted
parseMute :: String -> Bool
parseMute str = extractWord str 1 1 == "on"

formatVol :: Int -> Bool -> String
formatVol vol mute = concat [ "<fc=", colourise mute, ">"
                         , show vol
                         , "</fc>%"
                         ]
  where colourise True  = "red"
        colourise False = "green"

---- Network
  
-- TODO: Consider rationalising all this mess
readState :: String -> NetState
readState "connected"    = Connected
readState "disconnected" = Disconnected
readState "unavailable"  = Unavailable
readState str = if "connecting" `isPrefixOf` str
                  then Connecting
                  else Other str

formatNet :: [NetState] -> String -> String
formatNet [_, Connected] ip = "<fc=yellow>"++ip++"</fc>"
formatNet [Connected, _] ip = "<fc=green>"++ip++"</fc>"
formatNet [Disconnected, _] _ = "<fc=darkred>disconnected</fc>"
formatNet [Unavailable, _] _ = "<fc=darkred>unavailable</fc>"
formatNet [_, Connecting] _ = "<fc=yellow>connecting...</fc>"
formatNet [Connecting, _] _ = "<fc=orange>connecting...</fc>"
formatNet [Other x, _] _ = "<fc=blue>"++x++"</fc>"


-- Helpers

extractWord :: String -> Int -> Int -> String
extractWord str line word = words (lines str !! line) !! word

-- IO code

-- TODO: change to `ifconfig`
getIP :: IO String
getIP = do
   ip <- timeout 100000 $ readProcess "hostname" ["-i"] ""
   return . head . words $ fromMaybe "127.0.0.1" ip

getNetState :: IO [NetState]
getNetState = do
  devs <- readProcess "nmcli" ["dev"] ""
  return $ map (readState . flip (extractWord devs) 2) [1,2]

getVol :: IO Int
getVol = liftM parseVolume $ readProcess "amixer" ["get", "Master"] ""

getMute :: IO Bool
getMute = liftM parseMute $ readFile "/proc/acpi/ibm/volume"

getBat :: IO String
getBat = do
  output <- readProcess "acpi" ["-b"] ""
  return . batOutput . parseBat $ output

---- Main

main :: IO ()
main = do
  putStr "Bat: "
  getBat >>= putStr
  putStr " | Vol: "
  liftM2 formatVol getVol getMute >>= putStr
  putStr " | Net: "
  liftM2 formatNet getNetState getIP >>= putStr
