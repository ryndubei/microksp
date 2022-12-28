module Plot 
  ( windowWidth
  , windowHeight
  , background
  , plot ) where

import Graphics.Gloss
import Lib (Planet(..), Time, Velocity, Altitude, Vessel(..), atmosphereHeight, Density, burnTime)
import Data.Bifunctor (bimap)
import Data.List (nubBy, sortOn)
import Graphics.Gloss.Geometry.Angle (radToDeg)

background :: Color
background = black

windowWidth, windowHeight :: Float
windowWidth = 800
windowHeight = 800

skyHeight :: Float
skyHeight = windowHeight / 2 

groundHeight :: Float
groundHeight = windowHeight / 2

windowHeightMetres :: Planet -> Double
windowHeightMetres planet = 
  realToFrac (windowHeight / skyHeight) * atmosphereHeight planet

windowScale :: Planet -> Double
windowScale planet = realToFrac windowHeight / windowHeightMetres planet

pathColor :: Color
pathColor = red

atmosphereColor :: Planet -> Color
atmosphereColor planet =
  case planet of
    Kerbin -> blue
    Duna -> orange
    Eve -> magenta
    Laythe -> light blue
    Jool -> light green

groundColor :: Planet -> Color
groundColor planet =
  case planet of
    Kerbin -> dark (dark green)
    Duna -> dark red
    Eve -> dark violet
    Laythe -> greyN 0.2
    Jool -> dark green

canvas :: [(Altitude,Density)] -> Vessel -> Picture
canvas table vessel = groundSky table (currentPlanet vessel)

infoText :: Vessel -> Picture
infoText vessel = undefined
  where 
    texts =
      let timeText = text $ "Burn time :" ++ (show . round) (burnTime vessel) ++ "s"
          speedText = text $ "Gravity kick speed: " ++ (show . round) (gravityKickSpeed vessel) ++ "m/s"
          angleText = text $ "Gravity kick angle: " ++ (show . round . radToDeg . realToFrac) (gravityKickAngle vessel)
          thrustText = text $ "Engine thrust: " ++ (show . round) (engineForce vessel) ++ "N"
          massText = text $ "Starting mass: " ++ (show . round) (startingMass vessel) ++ "kg"
          deltavText = text $ "Delta-V: " ++ (show . round) (deltaV vessel) ++ "m/s"

      in [timeText,speedText,angleText, thrustText, massText]

textColor :: Color
textColor = black

groundSky :: [(Altitude,Density)] -> Planet -> Picture
groundSky atmosphereTable planet =
  let
    ground = 
      color gColor . translate 0 (-windowHeight / 4) 
      $ rectangleSolid windowWidth groundHeight
    sky = drawAtmosphere atmosphereTable planet
  in pictures [ground,sky]
  where
    gColor = groundColor planet

plot :: [(Altitude,Density)] -> Vessel -> [(Time,Velocity,Altitude)] -> Picture
plot atmosphereTable vessel flightPath = 
  let
    points = zip xs (map (\(_,_,h) -> h) flightPath) 
    pointsWindowScale = map (bimap (*windowScale planet) (*windowScale planet)) points
    pathWindow = 
      line $ map (bimap realToFrac realToFrac) pointsWindowScale
  in pictures [canvas atmosphereTable vessel,color pathColor $ translate (-0.4*windowWidth) 0 pathWindow]
  where
    planet = currentPlanet vessel
    dxs = zipWith 
      (\(t1,(v1,_),_) (t2,(v2,_),_) -> 0.5 * (t2-t1) * abs(v2-v1) + (t2-t1) * min v2 v1)
      flightPath (tail flightPath)
    xs = scanl (+) 0 dxs
    -- TODO: use more robust functions for finding the integral of velocity over time,
    -- like 1/2at^2+v0t - make sure to upgrade Simulation module too

drawAtmosphere :: [(Altitude,Density)] -> Planet -> Picture
drawAtmosphere atmosphereTable planet = 
  let
    zippedAltitudes = [ (alt1,alt2,d) | ((alt1,d),(alt2,_)) <- zip atmosphereTable' (tail atmosphereTable') ] 
  in pictures $ map (\(a1,a2,d) -> drawLayer a1 a2 d) zippedAltitudes
  where
    atmosphereTable' = nubBy (\a b -> fst a == fst b) . sortOn fst $ atmosphereTable
    initialDensity = (snd . head) atmosphereTable'
    drawLayer alt1 alt2 d = 
      color (withAlpha opacity (atmosphereColor planet)) . translate 0 centre $ rectangleSolid width height
      where
        opacity = realToFrac $ d / initialDensity
        alt1' = realToFrac $ windowScale planet * alt1
        alt2' = realToFrac $ windowScale planet * alt2
        width = windowWidth
        height = alt2' - alt1'
        centre = 0.5*(alt1'+alt2')