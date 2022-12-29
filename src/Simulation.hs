-- NOTE: At the moment this module does not consider the curvature of the planet.

module Simulation (fly, flyFromStart) where

import Lib
  ( Vessel(..)
  , Velocity
  , Time
  , Density
  , Altitude
  , Vector
  , burnTime
  , gFieldStrength, mulMatrixVector, rotMatrix, magV, mulSV, addV
  , toCentre, Planet, Position, negV, normaliseV, planetRadius, endMass )

-- | Unit vector corresponding to the x axis.
xAxis :: Vector
xAxis = (1,0)

-- | Unit vector corresponding to the y axis.
yAxis :: Vector
yAxis = (0,1)

-- | Unit vector corresponding to the local y axis.
localYAxis :: Planet -> Position -> Vector
localYAxis planet pos = (negV . normaliseV) (toCentre planet pos)

-- | Average length of a KSP physics tick in seconds.
constKSPPhysicsTick :: Double
constKSPPhysicsTick = 0.02

-- | Instantaneous rate of change of velocity, given the parameters.
-- Time is measured from start of burn.
-- If velocity = 0, the orientation of the rocket is assumed to be
-- up.
velocityDerivative :: Vessel -> Velocity -> Position -> Time -> Density -> Vector
velocityDerivative vessel v pos t d =
  let
    gVector = (-g) `mulSV` localYAxis planet pos
    currentMass = m0 - (thrust / vEx) * t
    thrustVector = thrust `mulSV` normaliseV v
    airResistanceVector = (-0.5 * cDA * d * magV v) `mulSV` v
  in
    if v == (0,0)
      then gVector `addV` ((1 / currentMass) `mulSV` (thrust `mulSV` localYAxis planet pos))
      else gVector `addV` ((1 / currentMass) `mulSV` (thrustVector `addV` airResistanceVector))
  where
    planet = currentPlanet vessel
    cDA = dragCoefficientArea vessel
    thrust = engineForce vessel
    vEx = exhaustVelocity vessel
    m0 = startingMass vessel
    pos' = negV (toCentre planet pos)
    h = magV pos' - planetRadius planet
    g = gFieldStrength planet h

-- | Increment current velocity by 1 physics tick
velocityStep :: Vessel -> Velocity -> Position -> Time -> Density -> Velocity
velocityStep vessel v pos t d =
  v `addV` mulSV constKSPPhysicsTick (velocityDerivative vessel v pos t d)

-- | Given a Vessel, initial conditions, and an Altitude -> Density function, 
-- return the list of tuples (Time,Velocity,Altitude) until the vessel
-- collides with the ground, or runs out of fuel, whichever comes first.
-- If the outOfFuel Bool is set as True, then the function will not check for not being out of fuel,
-- but will limit itself to only 20000 further ticks (at 0.02 seconds per tick this is 400 seconds)
fly :: Bool -> (Time,Velocity,Position) -> Vessel -> (Altitude -> Density) -> [(Time,Velocity,Position)]
fly outOfFuel initialConditions vessel f =
  let
    hasFuel (t,_,_) = t + constKSPPhysicsTick <= burnTime vessel
    aboveGround (_,_,pos) = magV (toCentre planet pos) >= planetRadius planet
    flight = takeWhile (\x -> (hasFuel x || outOfFuel) && aboveGround x) (iterate flyStep initialConditions)
  in (if outOfFuel then take 20000 else id) flight
  where
    planet = currentPlanet vessel
    flyStep :: (Time,Velocity,Position) -> (Time,Velocity,Position)
    flyStep (t,v,pos) = (t',v',pos `addV` dpos)
      where
        t' = t+constKSPPhysicsTick
        posFromPlanet = negV (toCentre planet pos)
        h = magV posFromPlanet - planetRadius planet
        v' = velocityStep vessel v pos t (f h)
        vavg = 0.5 `mulSV` (v' `addV` v)
        dpos = constKSPPhysicsTick `mulSV` vavg

vesselSpent :: Vessel -> Vessel
vesselSpent vessel = vessel { startingMass = endMass vessel, engineForce = 0, deltaV = 0}

-- | Given the parameters, return the list of tuples (Time,Velocity,Position)
-- until the vessel reaches space, reaches apoapsis, or runs out of fuel,
-- whichever comes first.
-- Includes a gravity kick and does not require initial conditions
-- (as they are taken from vessel data).
flyFromStart :: Vessel -> (Altitude -> Density) -> [[(Time,Velocity,Position)]]
flyFromStart vessel f =
  let
    launchBurn =
      takeWhile
      (\(time,vel,position) -> magV vel
        < gravityKickSpeed vessel
        + constKSPPhysicsTick * currentAcceleration time position)
      (fly False initialConditions vessel f)
    (t,v,pos) = last launchBurn
    v' = mulMatrixVector (rotMatrix (-gravityKickAngle vessel)) v
    gravityTurn = fly False (t,v',pos) vessel f
    freeFall = fly True (last gravityTurn) vessel' f
  in (init launchBurn ++ gravityTurn) : [freeFall]
  where
    vessel' = vesselSpent vessel
    planet = currentPlanet vessel
    initialConditions = (0,(0,0),(0,launchAltitude vessel))
    m0 = startingMass vessel
    thrust = engineForce vessel
    vEx = exhaustVelocity vessel
    g = gFieldStrength planet
    h pos = magV (toCentre planet pos) - planetRadius planet
    currentAcceleration t pos = (thrust / (m0 - thrust * t / vEx)) - g (h pos)

