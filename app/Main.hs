module Main (main) where

import Lib (Vessel(..), Planet(..))
import Plot (windowWidth, windowHeight, background, plot)
import Simulation (flyFromStart)
import AtmosphereData (densityFunction, densityTable)

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg)

window :: Display
window = InWindow "MicroKSP" (round windowWidth, round windowHeight) (0,0)

initialConditions :: Vessel
initialConditions = Vessel 
  { dragCoefficientArea = 5
  , engineForce = 1000000
  , exhaustVelocity = 2500
  , startingMass = 80000
  , launchAltitude = 75.684
  , currentPlanet = Kerbin
  , gravityKickAngle = realToFrac $ degToRad 10
  , gravityKickSpeed = 100
  , deltaV = 3400
  }

main :: IO ()
main = do
  atmosphereTable <- densityTable (currentPlanet initialConditions)
  f <- densityFunction (currentPlanet initialConditions)
  let flightData = flyFromStart initialConditions f
  let (lastt,(vx,vy),lastalt) = last flightData
  putStrLn $ "Final time: " ++ show lastt ++ "s"
  putStrLn $ "Final velocity: " ++ show (sqrt(vx*vx + vy*vy)) ++ "m/s"
  putStrLn $ "Final angle from vertical: " ++ show ((radToDeg . realToFrac) (atan (vx / vy)))
  putStrLn $ "Final altitude: " ++ show lastalt ++ "m"
  display window background (plot atmosphereTable initialConditions flightData)
