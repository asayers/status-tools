import System.Process (readProcess)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import System.Timeout (timeout)

data DevState = Connected | Connecting | Disconnected | Unavailable | Other String
  deriving (Show)
  
readState :: String -> DevState
readState "connected"    = Connected
readState "disconnected" = Disconnected
readState "unavailable"  = Unavailable
readState str = if "connecting" `isPrefixOf` str
                  then Connecting
                  else Other str

-- Returns [wifi, eth]
parseDevices :: String -> [DevState]
parseDevices str = map (readState . extract) [1,2]
  where extract n = words (lines str !! n) !! 2

output :: [DevState] -> String -> String
output [_, Connected] ip = "<fc=yellow>"++ip++"</fc>"
output [Connected, _] ip = "<fc=green>"++ip++"</fc>"
output [Disconnected, _] _ = "<fc=darkred>disconnected</fc>"
output [Unavailable, _] _ = "<fc=darkred>unavailable</fc>"
output [_, Connecting] _ = "<fc=yellow>connecting...</fc>"
output [Connecting, _] _ = "<fc=orange>connecting...</fc>"
output [Other x, _] _ = "<fc=blue>"++x++"</fc>"

safeIP :: IO String
safeIP = do
   ip <- timeout 10000 $ readProcess "hostname" ["-i"] ""
   return . head . words $ fromMaybe "127.0.0.1" ip

main :: IO ()
main = do
  devs <- liftM parseDevices $ readProcess "nmcli" ["dev"] ""
  ip   <- safeIP
  putStrLn $ output devs ip
