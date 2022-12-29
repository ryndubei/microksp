module Plot
  ( windowWidth
  , windowHeight
  , background
  , plot ) where

import Graphics.Gloss
import Lib (Planet(..), Time, Velocity, Altitude, Vessel(..), atmosphereHeight, Density, burnTime, planetRadius, Position, twr)
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
canvas table vessel = pictures 
  [groundSky table (currentPlanet vessel)
  , infoText vessel
  , spaceMarker (currentPlanet vessel)]

infoText :: Vessel -> Picture
infoText vessel = 
  scaleFinal (5 / realToFrac (imageScale vessel)) 
  . translate (-windowWidth) (2*textSpacing-windowHeight) 
  . color textColor 
  . pictures 
  $ orderTexts texts
  where
    texts =
      let planetText = text $ "Current planet: " ++ show (currentPlanet vessel)
          timeText = text $ "Burn time: " ++ (show . round) (burnTime vessel) ++ "s"
          speedText = text $ "Gravity kick speed: " ++ (show . round) (gravityKickSpeed vessel) ++ "m/s"
          angleText = text $ "Gravity kick angle: " ++ (show . (/10) . realToFrac . round . (*10) . radToDeg . realToFrac) (gravityKickAngle vessel)
          thrustText = text $ "Engine thrust: " ++ (show . round) (engineForce vessel) ++ "N"
          exhaustText = text $ "Engine exhaust velocity: " ++ (show . round) (exhaustVelocity vessel) ++ "m/s"
          massText = text $ "Vessel starting mass: " ++ (show . round) (startingMass vessel) ++ "kg"
          twrText = text $ "Thrust-to-weight ratio at sea level: " ++ (show . (/10) . realToFrac .  round . (*10)) (twr vessel)
          deltaVText = text $ "Vessel delta-V: " ++ (show . round) (deltaV vessel) ++ "m/s"
          launchAltText = text $ "Vessel launch altitude: " ++ (show . round) (launchAltitude vessel) ++ "m"
          dragText = text $ "Vessel drag coefficient*area: " ++ (show . (/100) . realToFrac . round . (*100)) (dragCoefficientArea vessel) ++ "m^2"
      in [planetText,timeText,speedText,angleText,thrustText,exhaustText,massText,twrText,deltaVText,launchAltText,dragText]
    textSpacing = 130
    orderTexts = zipWith (translate 0) (map (* (-textSpacing)) [0..])

spaceMarker :: Planet -> Picture
spaceMarker planet = 
  translate x y 
  . color yellow 
  $ circle 
  ( (realToFrac . (*windowScale planet) ) 
  (planetRadius planet + atmosphereHeight planet))
  where (x,y) = planetCentre planet

textColor :: Color
textColor = white

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

stageColors :: [Color]
stageColors = cycle [red,yellow,dark blue,green]

outOfFuelColor :: Color
outOfFuelColor = greyN 0.5

-- | Plot the flight path of the vessel over a canvas representing the planet the vessel is on.
plot :: [(Altitude,Density)] -> Vessel -> [[(Time,Velocity,Position)]] -> Picture
plot atmosphereTable vessel flightPath = 
  let stages = map plotStage (init flightPath)
      freeFall = plotStage (last flightPath)
  in (scaleFinal (realToFrac (imageScale vessel)) . pictures) $ canvas atmosphereTable vessel : zipWith color stageColors stages ++ [color outOfFuelColor freeFall]
  where
    planet = currentPlanet vessel
    plotStage stagePath =
      let points = map (\(_,_,pos) -> pos) stagePath
          pointsWindowScale = map (bimap (*windowScale planet) (*windowScale planet)) points
          pathWindow = line $ map (bimap realToFrac realToFrac) pointsWindowScale
      in translate launchPoint 0 pathWindow
        --scaleFinal $ pictures [canvas atmosphereTable vessel,color (head stageColors) $ translate launchPoint 0 pathWindow]

scaleFinal :: Float -> (Picture -> Picture)
scaleFinal n = scale (1 / n) (1 / n)

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
    -- ignore dead parts of function, it used to accept 3 parameters but got revised
    drawLayer _ alt2 d =
      let
        atmosphereCircle = translate x y $ circleSolid r2
        layerColor = withAlpha opacity (atmosphereColor planet)
      -- additional black circle is necessary for overriding the effects of the previous
      -- layer - make sure the top layer is drawn first!
      in pictures [color background atmosphereCircle, color layerColor atmosphereCircle]
      where
        (x,y) = planetCentre planet
        opacity = realToFrac $ d / initialDensity
        r2 = realToFrac $ windowScale planet * (alt2 + planetRadius planet)