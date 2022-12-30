-- Shared definitions for all other modules.
module Lib where

import Data.Set as S
import Graphics.Gloss.Interface.Pure.Game (Key)

-- | Datatype for planets with an atmosphere 
data Planet = Kerbin | Duna | Eve | Laythe | Jool deriving (Eq,Show,Enum,Bounded)

-- | Any near-unchanging simulation parameters go here.
data Vessel = Vessel 
  { dragCoefficientArea :: Double -- ^ Drag coefficient * cross-section area.
  , engineForce :: Double -- ^ Thrust of the vessel in a vacuum (N)
  , engineForceRatioASL :: Double -- ^ Engine force factor at 1 atm pressure
  , exhaustVelocity :: Double -- ^ The exhaust velocity of the engine (m/s)
  , startingMass :: Double -- ^ The total mass of the rocket at start (kg)
  , launchAltitude :: Double -- ^ The altitude of the launch site of the vessel (m)
  , currentPlanet :: Planet -- ^ The planet the rocket is on.
  , gravityKickAngle :: Double -- ^ The (clockwise) angle of the gravity kick (radians)
  , gravityKickSpeed :: Double -- ^ The speed at which the gravity kick is done (m/s)
  , deltaV :: Double -- ^ The maximum delta-V of the rocket (m/s)
  , keys :: S.Set Key -- ^ Keys defining simulation parameters to be changed on next update
  , imageScale :: Int -- ^ factor of image scale down from the default
  , orbitRefFrame :: Bool -- ^ Either surface or orbit reference frame for plotting
  }

-- | Datatype for both Vessel and its changing variables (time, velocity and
-- position).
data VesselState = VesselState
  { vessel :: Vessel -- ^ The generally static parameters of the vessel.
  , time :: Double -- ^ Time from start of burn. (s)
  , velocity :: Vector -- ^ The momentary velocity of the vessel. (R2 Vector, m/s)
  , altitude :: Double -- ^ The altitude of the vessel from sea level. (m)
  }

endMass :: Vessel -> Double
endMass vessel = startingMass vessel / exp (deltaV vessel / exhaustVelocity vessel)

burnTime :: Vessel -> Double
burnTime vessel = 
  let
    dpdt = engineForce vessel
    dmdt = dpdt / exhaustVelocity vessel
    dm = startingMass vessel - endMass vessel
  in dm / dmdt

twr :: Vessel -> Double
twr vessel = engineForce vessel / (gFieldStrength (currentPlanet vessel) 0 * startingMass vessel)

constStandardGravity :: Double
constStandardGravity = 9.80665

-- | 1 atm in pascals
const1Atm :: Double
const1Atm = 101325.0

engineForceAt :: Vessel -> (Pressure -> Double)
engineForceAt vessel pres = engineForce vessel - dthrust
  where
    presAtm = pres / const1Atm
    dthrust = (1 - engineForceRatioASL vessel) * presAtm

--------------------
-- Planet data
--------------------

-- | The standard gravitational parameter (GM) of a given planet in m^3/s^2
planetMU :: Planet -> Double
planetMU planet =
  case planet of
    Kerbin -> 3.5316e+12
    Duna -> 3.0136321e+11
    Eve -> 8.1717302e+12
    Laythe -> 1.962e+12
    Jool -> 2.82528e+14

-- | The equatorial radius of a planet in m
planetRadius :: Planet -> Double
planetRadius planet =
  case planet of
    Kerbin -> 600000.0
    Duna -> 320000.0
    Eve -> 700000.0
    Laythe -> 500000.0
    Jool -> 6000000.0

-- | The height of the atmosphere on a planet from sea level (m)
atmosphereHeight :: Planet -> Double
atmosphereHeight planet =
  case planet of
    Kerbin -> 70000.0
    Duna -> 50000.0
    Eve -> 90000.0
    Laythe -> 50000.0
    Jool -> 200000.0

-- | Return the molar mass of dry air at a particular planet, in kg/mole.
molarMass :: Planet -> Double
molarMass planet =
  case planet of
    Kerbin -> 0.0289644
    Duna -> 0.042
    Eve -> 0.043
    Laythe -> 0.0289644
    Jool -> 0.0022

-- | Returns the rotation period of a planet in seconds.
rotationPeriod :: Planet -> Double
rotationPeriod planet =
  case planet of
    Kerbin -> 21549.425
    Duna -> 65517.859
    Eve -> 80500.0
    Laythe -> 52980.879
    Jool -> 36000.0

-- | Returns the magnitude of the velocity of the planet's rotation in metres 
-- at the given altitude.
rotationVelocity :: Planet -> Altitude -> Double
rotationVelocity planet h =
  let angularVelocity = (2*pi) / rotationPeriod planet
      r = planetRadius planet + h
  in r * angularVelocity

-- | Change from orbit to surface reference frame for velocity
orbitToSurface :: Planet -> Position -> Velocity -> Velocity
orbitToSurface planet pos v =
  let h = altitudeAt planet pos
      vRotMag = rotationVelocity planet h
      rotVector = vRotMag `mulSV` localXAxis planet pos
  in v `subV` rotVector

-- | Change from surface to orbit reference frame for velocity
surfaceToOrbit :: Planet -> Position -> Velocity -> Velocity
surfaceToOrbit planet pos v =
  let h = altitudeAt planet pos
      vRotMag = rotationVelocity planet h
      rotVector = vRotMag `mulSV` localXAxis planet pos
  in v `addV` rotVector

-- | Return the angle that the planet has rotated by at the specified time.
hasRotatedBy :: Planet -> Time -> Double
hasRotatedBy planet t =
  let angularVelocity = (2*pi) / rotationPeriod planet
  in angularVelocity * t

-- | Rotate a vector by the given angle in radians around the given position,
-- counter-clockwise.
rotateAroundBy :: Position -> Vector -> Double -> Vector
rotateAroundBy pos v angle =
  let v' = v `subV` pos
  in mulMatrixVector (rotMatrix angle) v' `addV` pos

-- | Given a planet and an altitude above sea level, give the gravitational
-- field strength at that altitude.
gFieldStrength :: Planet -> Altitude -> Double
gFieldStrength planet h = planetMU planet / (r*r)
  where r = planetRadius planet + h

-- | Given a Planet and an altitude above sea level, find the orbital velocity
-- for a circular orbit at that altitude.
orbitalVel :: Planet -> Double -> Double
orbitalVel planet h = sqrt(planetMU planet / (h + planetRadius planet))

-- | Given a Planet and a Position, return the vector in the direction of the
-- planet's centre from that position.
toCentre :: Planet -> Position -> Vector
toCentre planet pos =
  let radius = planetRadius planet
      centre = (0,-radius)
  in centre `subV` pos

altitudeAt :: Planet -> Position -> Double
altitudeAt planet pos =
  let r = magV (toCentre planet pos)
  in r - planetRadius planet

--------------------
-- Type aliases
--------------------

type Vector = (Double,Double)
type Velocity = Vector
type Position = Vector

type Altitude = Double
type Temperature = Double
type Pressure = Double
type Density = Double
type Time = Double

-- | Each vector corresponds to a column
type Matrix2x2 = (Vector,Vector)

--------------------
-- Vector math functions
--------------------

addV :: Vector -> Vector -> Vector
addV (x1,y1) (x2,y2) = (x1+x2,y1+y2)

negV :: Vector -> Vector
negV (x,y) = (-x,-y)

subV :: Vector -> Vector -> Vector
subV v1 v2 = v1 `addV` negV v2

mulSV :: Double -> Vector -> Vector
mulSV s (x,y) = (s*x,s*y)

magV :: Vector -> Double
magV (x,y) = sqrt (x*x + y*y)

magVSquared :: Vector -> Double
magVSquared (x,y) = x*x + y*y

mulMatrixVector :: Matrix2x2 -> Vector -> Vector
mulMatrixVector (c1,c2) (a,b) = mulSV a c1 `addV` mulSV b c2

-- | Counterclockwise rotation matrix for a given angle
rotMatrix :: Double -> Matrix2x2
rotMatrix angle = ((cos angle,sin angle),(-sin angle,cos angle))

normaliseV :: Vector -> Vector
normaliseV v = (1 / magV v) `mulSV` v

dotV :: Vector -> Vector -> Double
dotV (x1,y1) (x2,y2) = x1*x2 + y1*y2

angleV :: Vector -> Vector -> Double
angleV v1 v2 = acos (v1 `dotV` v2 / (magV v1 * magV v2))

-- | Unit vector corresponding to the x axis.
xAxis :: Vector
xAxis = (1,0)

-- | Unit vector corresponding to the y axis.
yAxis :: Vector
yAxis = (0,1)

-- | Unit vector corresponding to the local y axis.
localYAxis :: Planet -> Position -> Vector
localYAxis planet pos = (negV . normaliseV) (toCentre planet pos)

localXAxis :: Planet -> Position -> Vector
localXAxis planet pos =
  let y = localYAxis planet pos
  in mulMatrixVector (rotMatrix (-pi / 2)) y

-- | Projection of a vector v onto vector u (u comes first)
projV :: Vector -> Vector -> Vector
projV u v = ((u `dotV` v) / (u `dotV` u)) `mulSV` u