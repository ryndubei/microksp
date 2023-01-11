module Main (main) where

import Lib (Vessel(..), Planet(..), Stage(..))
import Plot (windowWidth, windowHeight, background, plot)
import Simulation (flyFromStart)
import AtmosphereData (densityTable, defaultDensityTable, tableToFunction)
import Input (handleKeys, update)
import qualified Data.Set as S

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle (degToRad)
import Control.Exception (catch, IOException)
import Data.Maybe (fromJust)
import System.IO (stderr, hPutStrLn)

window :: Display
window = InWindow "MicroKSP" (round windowWidth, round windowHeight) (0,0)

initialConditions :: Vessel
initialConditions = Vessel
  { dragCoefficientArea = 10.0
  , engineForce = 988000
  , engineForceRatioASL = 0.78 -- does nothing for now
  , exhaustVelocity = 2796
  , startingMass = 53475
  , launchAltitude = 75.684
  , currentPlanet = Kerbin
  , gravityKickAngle = realToFrac $ degToRad 15.0
  , gravityKickSpeed = 150
  , deltaV = 1155
  , keys = S.empty
  , imageScale = 1
  , orbitRefFrame = False
  , followingStages = [Stage {stageThrust = 100000, stageMass = 10000, stageIsp = 350, stageDeltaV = 2000, stageDragArea = 5.0}]
  }

main :: IO ()
main = do
  drawFlight <- getDrawFlight
  play window background 10 initialConditions drawFlight handleKeys update

getDrawFlight :: IO (Vessel -> Picture)
getDrawFlight = do
  atmosphereTables <- zip planets <$> mapM perhapsDensityTable planets
  let atmosphereTable vessel = (fromJust . lookup planet) atmosphereTables where planet = currentPlanet vessel
  return (\v -> drawFlight (atmosphereTable v) v)
  where
    drawFlight atmosphereTable vessel = 
      let atmosphereFunction = tableToFunction atmosphereTable
          flightData = flyFromStart vessel atmosphereFunction
      in plot atmosphereTable vessel flightData
    planets = [toEnum 0 ..]
    -- If the density table could not be obtained, use the default table ([(0,0)])
    perhapsDensityTable planet = 
      catch (densityTable planet) 
      (\e -> 
        hPutStrLn stderr ("Failed to read atmosphere data: " ++ show (e :: IOException)) 
        >> return defaultDensityTable )
