import System.Process

type State = (Int, Power)
data Power = Charging | Discharging

-- Parse the output from `acpi`

parsePercent :: String -> Int
parsePercent str = read . takeWhile (/= '%') $ str

parsePower :: String -> Power
parsePower str = if str == "Charging,"
                   then Charging
                   else Discharging
                  
parse :: String -> State
parse str = (percent, power)
  where percent = parsePercent $ fields !! 3
        power   = parsePower   $ fields !! 2
        fields  = words str

-- Generate output

stateColour :: State -> String
stateColour (_, Charging) = "yellow"
stateColour (x, Discharging)
  | x < 10    = "red"
  | otherwise = "green"

output :: State -> String
output state =
  concat [ "<fc=", colour, ">"
         , percent
         , "</fc>%"
         ]
  where percent = show . fst $ state
        colour  = stateColour state

main :: IO ()
main = readProcess "acpi" ["-b"] "" >>= putStrLn . output . parse
