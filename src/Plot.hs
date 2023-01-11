{-# OPTIONS_GHC -Wno-type-defaults #-}
module Plot ( windowWidth, windowHeight, background, plot ) where

import Graphics.Gloss
import Lib
  ( Planet(..), Time, Velocity, Altitude, Vessel(..), atmosphereHeight, Density
  , burnTime, planetRadius, Position, twr, toCentre, angleV, magV, altitudeAt
  , orbitalVel, endMass, hasRotatedBy, orbitToSurface, rotateAroundBy )
import Data.Bifunctor ( bimap )
import Data.List ( nubBy, sortOn, find )
import Graphics.Gloss.Geometry.Angle ( radToDeg )

background :: Color
background = black

windowWidth, windowHeight :: Float
windowWidth = 800
windowHeight = 800

-- | Plot the flight path of the vessel over a canvas representing the planet the vessel is on.
plot :: [(Altitude,Density)] -> Vessel -> [[(Time,Velocity,Position)]] -> Picture
plot atmosphereTable vessel flightPath =
  let stages = map plotStage (init flightPath)
      freeFall = plotStage (last flightPath)
  in (scaleFinal (realToFrac (imageScale vessel)) . pictures)
    $ canvas atmosphereTable vessel
    -- Ignore the gravity turn for the apsis info
    : flightInfoText vessel (last flightPath)
    : zipWith color stageColors stages
    ++ [color outOfFuelColor freeFall]
  where
    planet = currentPlanet vessel
    plotStage stagePath =
      let stagePath' = if orbitRefFrame vessel then stagePath else map (compensateForRotation planet) stagePath
          points = map (\(_,_,pos) -> pos) stagePath'
          pointsWindowScale = map (bimap (*windowScale planet) (*windowScale planet)) points
          pathWindow = line $ map (bimap realToFrac realToFrac) pointsWindowScale
      in translate launchPoint 0 pathWindow

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
    Kerbin -> light blue
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
  makeTextLines (-windowWidth,3.5*textSpacing-windowHeight) 5 textSpacing vessel texts
  where
    texts =
      let planetText = "Current planet: " ++ show (currentPlanet vessel)
          timeText = "Burn time: " ++ (show . round) (burnTime vessel) ++ "s"
          speedText = "Gravity kick speed: " ++ (show . round) (gravityKickSpeed vessel) ++ "m/s"
          angleText = "Gravity kick angle: " ++ (show . (/10) . realToFrac . round . (*10) . radToDeg . realToFrac) (gravityKickAngle vessel)
          thrustText = "Engine thrust: " ++ (show . round) (engineForce vessel) ++ "N"
          exhaustText = "Engine exhaust velocity: " ++ (show . round) (exhaustVelocity vessel) ++ "m/s"
          massText = "Vessel starting mass: " ++ (show . round) (startingMass vessel) ++ "kg"
          twrText = "Thrust-to-weight ratio at sea level: " ++ (show . (/10) . realToFrac .  round . (*10)) (twr vessel)
          endMassText = "Vessel end mass: " ++ (show . round) (endMass vessel) ++ "kg"
          deltaVText = "Vessel delta-V: " ++ (show . round) (deltaV vessel) ++ "m/s"
          launchAltText = "Vessel launch altitude: " ++ (show . round) (launchAltitude vessel) ++ "m"
          dragText = "Vessel drag coefficient*area: " ++ (show . (/100) . realToFrac . round . (*100)) (dragCoefficientArea vessel) ++ "m^2"
          refFrameText = "Current reference frame: " ++ (if orbitRefFrame vessel then "Orbit" else "Surface")
      in [planetText,timeText,speedText,angleText,thrustText,exhaustText,massText,twrText,endMassText,deltaVText,launchAltText,dragText, refFrameText]
    textSpacing = 130

-- | Turn a list of Strings into lines of text as a picture, translated, scaled
-- and spaced according to the given parameters.
makeTextLines :: (Float,Float) -> Float -> Float -> Vessel -> [String] -> Picture
makeTextLines (x,y) n textSpacing vessel strs =
  scaleFinal (n / realToFrac (imageScale vessel))
  . translate x y
  . color textColor
  . pictures
  . orderTexts
  $ map text strs
  where
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
stageColors = cycle [red,yellow,green]

outOfFuelColor :: Color
outOfFuelColor = greyN 0.5

scaleFinal :: Float -> (Picture -> Picture)
scaleFinal n = scale (1 / n) (1 / n)

drawAtmosphere :: [(Altitude,Density)] -> Planet -> Picture
drawAtmosphere atmosphereTable planet =
  let
    -- reversed to start from top atmosphere layer
    zippedAltitudes = reverse [ (alt1,alt2,d) | ((alt1,d),(alt2,_)) <- zip atmosphereTable' (tail atmosphereTable') ]
  in pictures $ map (\ (a1,a2,d) -> drawLayer a1 a2 d) zippedAltitudes
  where
    -- atmosphere table might have duplicates or be unsorted so we have to do this
    atmosphereTable' = nubBy (\ a b -> fst a == fst b) . sortOn fst $ atmosphereTable
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

findApsis :: Planet -> [(Time,Velocity,Position)] -> Maybe (Time,Velocity,Position)
findApsis planet flightPath =
  let flightPointPairs = zip flightPath (tail flightPath)
      apsis = find (uncurry isApsis) flightPointPairs
  in fmap fst apsis
  where
    angleHorizon v pos = angleV v (toCentre planet pos) - (pi / 2)
    isApsis (_,v1,pos1) (_,v2,pos2) =
      let angle1 = angleHorizon v1 pos1
          angle2 = angleHorizon v2 pos2
      in 
        signum angle1 /= signum angle2

flightInfoText :: Vessel -> [(Time,Velocity,Position)] -> Picture
flightInfoText vessel flightPath = makeTextLines (-2.6*windowWidth,2.6*windowHeight) 6 130 vessel texts
  where
    planet = currentPlanet vessel
    maybeApsis = findApsis planet flightPath
    apTime = fmap (\(t,_,_) -> t) maybeApsis
    apVel = fmap (\(_,v,_) -> v) maybeApsis
    apPos = fmap (\(_,_,pos) -> pos) maybeApsis
    aph = fmap (altitudeAt planet) apPos
    orbVel = fmap (orbitalVel planet) aph
    deltaVToOrbit = do
      v2 <- orbVel
      v1 <- magV <$> apVel
      return (v2 - v1)
    texts =
      let altText = "Apsis altitude: " ++ showNA "m" (fmap round aph)
          velText = "Apsis velocity: " ++ showNA "m/s" (fmap (round . magV) apVel)
          timeText = "Time to apsis from launch: " ++ showNA "s" (fmap round apTime)
          deltaVText = "Delta-V to circularize: " ++ showNA "m/s" (fmap round deltaVToOrbit)
      in [altText,velText,timeText,deltaVText]
    showNA str = maybe "N/A" (\x -> show x ++ str)

-- | Convert both flight velocity and position to surface reference frame based on the time
-- from launch
compensateForRotation :: Planet -> (Time,Velocity,Position) -> (Time,Velocity,Position)
-- Assumes planet centre is at (0,-planetRadius planet)
compensateForRotation planet (t,v,pos) = (t,v',pos')
  where 
    angularDisplacement = hasRotatedBy planet t
    v' = orbitToSurface planet pos v
    pos' = rotateAroundBy (0,-planetRadius planet) pos angularDisplacement
    