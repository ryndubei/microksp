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
  , toCentre, Position, negV, normaliseV, planetRadius, endMass
  , atmosphereHeight, magVSquared, localYAxis, orbitToSurface, surfaceToOrbit )

-- | Average length of a KSP physics tick in seconds.
constKSPPhysicsTick :: Double
constKSPPhysicsTick = 0.02

-- | Instantaneous rate of change of velocity, given the parameters.
-- Time is measured from start of burn.
-- Velocity is relative to the surface!
-- If the burnStraightUp boolean parameter is True, the thrust vector will always point in the
-- local Y axis. Likewise, if velocity is 0, the "prograde" is assumed to be in the local Y axis
-- and so it will be the direction of the thrust vector.
velocityDerivative :: Bool -> Vessel -> Velocity -> Position -> Time -> Density -> Vector
velocityDerivative burnStraightUp vessel v pos t d =
  let
    outOfAtmosphere = magVSquared (toCentre planet pos) >= (atmosphereHeight planet + planetRadius planet)^2
    gVector = (-g) `mulSV` localYAxis planet pos
    currentMass = m0 - (thrust / vEx) * t
    thrustVector = thrust `mulSV` (if burnStraightUp then localYAxis planet pos else normaliseV v) 
    -- (untested) performance optimization: if out of atmosphere, air resistance is 0.
    airResistanceVector = if outOfAtmosphere
      then (0,0)
      else (-0.5 * cDA * d * magV v) `mulSV` v
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

-- | Increment current velocity by 1 physics tick. Both the taken and returned
-- velocities are in the orbit reference frame!
velocityStep :: Bool -> Vessel -> Velocity -> Position -> Time -> Density -> Velocity
velocityStep burnStraightUp vessel v pos t d =
  surfaceToOrbit planet pos (v' `addV` dv')
  where
    planet = currentPlanet vessel
    v' = orbitToSurface planet pos v
    dv' = mulSV constKSPPhysicsTick (velocityDerivative burnStraightUp vessel v' pos t d)

-- | Given a Vessel, initial conditions, and an Altitude -> Density function, 
-- return the list of tuples (Time,Velocity,Altitude) until the vessel
-- collides with the ground, or runs out of fuel, whichever comes first.
-- If the outOfFuel Bool is set as True, then the function will not check for not being out of fuel,
-- but will limit itself to only 40000 further ticks (at 0.02 seconds per tick this is 800 seconds)
-- The burnStraightUp bool is passed down to the velocityDerivative function. It exists for the purpose
-- of not tipping over while in the middle of an initial launch burn.
fly :: Bool -> Bool -> (Time,Velocity,Position) -> Vessel -> (Altitude -> Density) -> [(Time,Velocity,Position)]
fly burnStraightUp outOfFuel initialConditions vessel f =
  let
    hasFuel (t,_,_) = t + constKSPPhysicsTick <= burnTime vessel
    aboveGround (_,_,pos) = magV (toCentre planet pos) >= planetRadius planet
    flight = takeWhile (\x -> (hasFuel x || outOfFuel) && aboveGround x) (iterate flyStep initialConditions)
  in (if outOfFuel then take 40000 else id) flight
  where
    planet = currentPlanet vessel
    flyStep :: (Time,Velocity,Position) -> (Time,Velocity,Position)
    flyStep (t,v,pos) = (t',v',pos `addV` dpos)
      where
        t' = t+constKSPPhysicsTick
        posFromPlanet = negV (toCentre planet pos)
        h = magV posFromPlanet - planetRadius planet
        v' = velocityStep burnStraightUp vessel v pos t (f h)
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
      (\(time,vel,position) -> magV (orbitToSurface planet position vel)
        < gravityKickSpeed vessel
        + constKSPPhysicsTick * currentAcceleration time position)
      (fly True False initialConditions vessel f)
    (t,v,pos) = last launchBurn
    vSurfMag = magV (orbitToSurface planet pos v)
    vSurfMagUp = vSurfMag `mulSV` localYAxis planet pos
    v' = surfaceToOrbit planet pos 
      $ mulMatrixVector (rotMatrix (-(gravityKickAngle vessel))) vSurfMagUp
    gravityTurn = fly False False (t,v',pos) vessel f
    freeFall = fly False True (last gravityTurn) vessel' f
  in (init launchBurn ++ gravityTurn) : [freeFall]
  where
    vessel' = vesselSpent vessel
    planet = currentPlanet vessel
    startpos = (0,launchAltitude vessel)
    startvel = surfaceToOrbit planet startpos (0,0)
    initialConditions = (0,startvel,startpos)
    m0 = startingMass vessel
    thrust = engineForce vessel
    vEx = exhaustVelocity vessel
    g = gFieldStrength planet
    h pos = magV (toCentre planet pos) - planetRadius planet
    currentAcceleration t pos = (thrust / (m0 - thrust * t / vEx)) - g (h pos)

