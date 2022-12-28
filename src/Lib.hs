-- Shared definitions for all other modules.
module Lib where

import Data.Set as S
import Graphics.Gloss.Interface.Pure.Game (Key)

-- | Datatype for planets with an atmosphere 
data Planet = Kerbin | Duna | Eve | Laythe | Jool deriving (Eq,Show,Enum,Bounded)

-- | Any near-unchanging simulation parameters go here.
data Vessel = Vessel 
  { dragCoefficientArea :: Double -- ^ Drag coefficient * cross-section area.
  , engineForce :: Double -- ^ Thrust of the vessel (N)
  , exhaustVelocity :: Double -- ^ The exhaust velocity of the engine (m/s)
  , startingMass :: Double -- ^ The total mass of the rocket at start (kg)
  , launchAltitude :: Double -- ^ The altitude of the launch site of the vessel (m)
  , currentPlanet :: Planet -- ^ The planet the rocket is on.
  , gravityKickAngle :: Double -- ^ The (clockwise) angle of the gravity kick (radians)
  , gravityKickSpeed :: Double -- ^ The speed at which the gravity kick is done (m/s)
  , deltaV :: Double -- ^ The maximum delta-V of the rocket (m/s)
  , keys :: S.Set Key -- ^ Keys defining simulation parameters to be changed on next update
  , imageScale :: Int -- ^ factor of image scale down from the default
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

constStandardGravity :: Double
constStandardGravity = 9.80665

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

-- | Given a planet and an altitude above sea level, give the gravitational
-- field strength at that altitude.
gFieldStrength :: Planet -> Altitude -> Double
gFieldStrength planet h = planetMU planet / (r*r)
  where r = planetRadius planet + h

--------------------
-- Type aliases
--------------------

type Vector = (Double,Double)
type Velocity = Vector

type Altitude = Double
type Temperature = Double
type Pressure = Double
type Density = Double
type Time = Double

-- | Each vector corresponds to a column
type Matrix2x2 = (Vector,Vector)
