-- Shared definitions for all other modules.
module Lib where

-- | Datatype for planets with an atmosphere 
data Planet = Kerbin | Duna | Eve | Laythe | Jool deriving (Eq,Show)

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
-- TODO: add more data
planetMU planet =
  case planet of
    Kerbin -> 3.5316e+12
    Duna -> undefined
    Eve -> undefined
    Laythe -> undefined
    Jool -> undefined

-- | The equatorial radius of a planet in m
planetRadius :: Planet -> Double
planetRadius planet =
  case planet of
    Kerbin -> 600000.0
    Duna -> undefined
    Eve -> undefined
    Laythe -> undefined
    Jool -> undefined

-- | The height of the atmosphere on a planet from sea level (m)
atmosphereHeight :: Planet -> Double
atmosphereHeight planet =
  case planet of
    Kerbin -> 70000.0
    Duna -> undefined
    Eve -> undefined
    Laythe -> undefined
    Jool -> undefined

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
