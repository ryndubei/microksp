-- NOTE: you must have atmosphere data at atmosphereDataPath in the form
-- "<lowercase planet name>_atmosphere.txt" containing lines with 
-- "<alt>m <temp>K <pres>P" to retrieve data for a given planet.
-- It doesn't actually necessarily have to be m, K, P - it can be any
-- character at the end of each value, it's just clearer this way.
-- However, it does have to be in this exact order.
-- The altitude doesn't have to be in ascending order, or contain no duplicates
-- (in case there is a duplicate, the first result is taken)

module AtmosphereData (densityFunction, densityTable, tableToFunction, defaultDensityTable) where

import Lib ( Planet(..), Altitude, Temperature, Pressure, Density, molarMass, constGas )
import Data.Char (toLower)
import Data.List (sortOn, nubBy, findIndex)
import Paths_microksp (getDataFileName)
import Data.Maybe ( fromJust, isJust )

defaultDensityTable :: [(Altitude,Density)]
defaultDensityTable = [(0,0)]

-- | Given a Planet, return an IO function taking any Altitude and returning
-- the corresponding density. Works by just connecting the dots on an
-- altitude/density scatter plot.
densityFunction :: Planet -> IO (Altitude -> Density)
densityFunction planet = do
  table <- densityTable planet
  let f = tableToFunction table
  return f

-- | Given a table, convert it to a function. In case of duplicate first term,
-- the first occurence takes priority. The table does not have to be ordered.
tableToFunction :: (Fractional a, Ord a) => [(a,a)] -> (a -> a)
tableToFunction table x
  | isJust $ lookup x table = fromJust $ lookup x table
  | x < (fst . head) table' = (snd . head) table'
  | x > (fst . last) table' = (snd . last) table'
  | otherwise = 
    let 
      i2 = fromJust $ findIndex (>x) (map fst table')
      i1 = i2 - 1
    in lineFunction (table'!!i1) (table'!!i2) x
  where
    table' = nubBy (\a b -> fst a == fst b) $ sortOn fst table

-- | Create a line function between two points. x1 and x2 must not be equal.
lineFunction :: (Fractional a, Ord a) => (a,a) -> (a,a) -> (a -> a)
lineFunction (x1,y1) (x2,y2) x
  | x1 == x2 = error "Infinite gradient"
  | otherwise = m*x + c
  where 
    m = (y2 - y1) / (x2 - x1)
    c = y1 - m*x1

-- | Given a Planet, process the temperature and pressure data for that planet
-- stored at atmosphereDataDirectory into a list of (Altitude,Density) pairs.
densityTable :: Planet -> IO [(Altitude,Density)]
densityTable planet = do
  atmosphereFilePath <- getDataFileName atmosphereFile
  dataLines <- lines <$> readFile atmosphereFilePath
  let densityLines = map (processDataLine planet) dataLines
  return densityLines
  where
    planetName = map toLower (show planet)
    atmosphereFile = "atmosphere/" ++ planetName ++ "_atmosphere.txt"

-- | Turn a raw data line into an (Altitude,Density) pair, given the Planet.
processDataLine :: Planet -> String -> (Altitude,Density)
processDataLine planet str = (alt,atmosphericDensity planet temp pres)
  where 
    (alt,temp,pres) = extractAltTempPres str

-- | Given a planet, return the density at the given temperature and pressure.
atmosphericDensity :: Planet -> Temperature -> Pressure -> Density
atmosphericDensity planet t p = p / (rSpecific * t)
  where
    rSpecific = constGas / molarMass planet

-- | Given a column containing values for altitude, temperature and pressure,
-- extract their values into a tuple as Doubles.
extractAltTempPres :: String -> (Altitude,Temperature,Pressure)
extractAltTempPres str = 
  case values of
    [alt,temp,pres] -> (alt,temp,pres)
    _ -> error "Atmosphere data file in invalid format"
  where
    strs = words str
    values = map (read . init) strs -- last char is ignored because it's the unit
