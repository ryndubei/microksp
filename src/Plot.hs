module Plot 
  ( windowWidth
  , windowHeight
  , background
  , plot ) where

import Graphics.Gloss
import Lib (Planet(..), Time, Velocity, Altitude, Vessel(..), atmosphereHeight, Density, burnTime, planetRadius)
import Data.Bifunctor (bimap, Bifunctor (first))
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

launchPoint :: Float
launchPoint = -0.4 * windowWidth

planetCentre :: Planet -> (Float,Float)
planetCentre planet = 
  let size = realToFrac $ windowScale planet * planetRadius planet
  in (launchPoint, -size)

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
    Duna -> light red
    Eve -> violet
    Laythe -> greyN 0.4
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
      in [timeText,speedText,angleText, thrustText]

textColor :: Color
textColor = black

groundSky :: [(Altitude,Density)] -> Planet -> Picture
groundSky atmosphereTable planet =
  let
    ground = drawPlanet planet
    sky = drawAtmosphere atmosphereTable planet
  in pictures [sky, ground]

drawPlanet :: Planet -> Picture
drawPlanet planet =
  let size = realToFrac $ windowScale planet * planetRadius planet
      (x,y) = planetCentre planet
      planetCircle = color (groundColor planet) $ circleSolid size
  in translate x y planetCircle

-- | Plot the flight path of the vessel over a canvas representing the planet the vessel is on.
plot :: [(Altitude,Density)] -> Vessel -> [(Time,Velocity,Altitude)] -> Picture
plot atmosphereTable vessel flightPath = 
  let
    points = zip xs (map (\(_,_,h) -> h) flightPath) 
    pointsWindowScale = map (bimap (*windowScale planet) (*windowScale planet)) points
    pathWindow = 
      line $ map (bimap realToFrac realToFrac) pointsWindowScale
  in scaleFinal $ pictures [canvas atmosphereTable vessel,color pathColor $ translate launchPoint 0 pathWindow]
  where
    scaleFinal = scale (1 / realToFrac (imageScale vessel)) (1 / realToFrac (imageScale vessel))
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
    -- reversed to start from top atmosphere layer
    zippedAltitudes = reverse [ (alt1,alt2,d) | ((alt1,d),(alt2,_)) <- zip atmosphereTable' (tail atmosphereTable') ] 
  in pictures $ map (\(a1,a2,d) -> drawLayer a1 a2 d) zippedAltitudes
  where
    -- atmosphere table might have duplicates or be unsorted so we have to do this
    atmosphereTable' = nubBy (\a b -> fst a == fst b) . sortOn fst $ atmosphereTable
    initialDensity = (snd . head) atmosphereTable'
    -- ignore dead parts of function, it went through several revisions and works fine right now
    drawLayer _ alt2 d = 
      let 
        atmosphereCircle = translate x y $ circleSolid r2 
      -- additional black circle is necessary for preventing upper layers from layering on top - make sure
      -- drawLayer is applied to the topmost layer first so the layers below don't get drawn over!
      in pictures [color background atmosphereCircle, color layerColor atmosphereCircle]
      where
        (x,y) = planetCentre planet
        opacity = realToFrac $ d / initialDensity
        r2 = realToFrac $ windowScale planet * (alt2 + planetRadius planet)
        layerColor = withAlpha opacity (atmosphereColor planet)