-- NOTE: At the moment this module does not consider the curvature of the planet.

module Simulation (fly, flyFromStart) where

import Lib
  ( Vessel(..)
  , Velocity
  , Time
  , Density
  , Altitude
  , Vector
  , Matrix2x2, burnTime
  , atmosphereHeight
  , gFieldStrength )

-- | Unit vector corresponding to the x axis.
xAxis :: Vector
xAxis = (1,0)

-- | Unit vector corresponding to the y axis.
yAxis :: Vector
yAxis = (0,1)

-- | Average length of a KSP physics tick in seconds.
constKSPPhysicsTick :: Double
constKSPPhysicsTick = 0.02

-- | Instantaneous rate of change of velocity, given the parameters.
-- Time is measured from start of burn.
-- If velocity = 0, the orientation of the rocket is assumed to be
-- up.
-- Gravity is always assumed to be straight down - this has to be accounted for.
velocityDerivative :: Vessel -> Velocity -> Altitude -> Time -> Density -> Vector
velocityDerivative vessel v h t d =
  let
    gVector = (-g) `mulSV` yAxis
    currentMass = m0 - (thrust / vEx) * t
    thrustVector = (thrust / magV v) `mulSV` v
    airResistanceVector = (0.5 * cDA * d * magV v) `mulSV` v
  in
    if v == (0,0)
      then (0,(thrust / currentMass) - g)
    else
      gVector `addV` ((1 / currentMass) `mulSV` (thrustVector `subV` airResistanceVector))
  where
    planet = currentPlanet vessel
    cDA = dragCoefficientArea vessel
    thrust = engineForce vessel
    vEx = exhaustVelocity vessel
    m0 = startingMass vessel
    g = gFieldStrength planet h

-- | Increment current velocity by 1 physics tick
velocityStep :: Vessel -> Velocity -> Altitude -> Time -> Density -> Velocity
velocityStep vessel v h t d =
  v `addV` mulSV constKSPPhysicsTick (velocityDerivative vessel v h t d)

-- | Given a Vessel, initial conditions, and an Altitude -> Density function, 
-- return the list of tuples (Time,Velocity,Altitude) until the vessel reaches
-- space, reaches apoapsis, or runs out of fuel, whichever comes first.
fly :: (Time,Velocity,Altitude) -> Vessel -> (Altitude -> Density) -> [(Time,Velocity,Altitude)]
fly initialConditions vessel f =
  let
    hasFuel (t,_,_) = t + constKSPPhysicsTick <= burnTime vessel
    notPastApoapsis (_,(_,vy),_) = vy >= 0
    notInSpace (_,_,h) = h < atmosphereHeight (currentPlanet vessel)
  in takeWhile hasFuel (iterate flyStep initialConditions)
  where
    flyStep :: (Time,Velocity,Altitude) -> (Time,Velocity,Altitude)
    flyStep (t,v,h) = (t',v',h+sy)
      where
        (_,vy) = v
        t' = t+constKSPPhysicsTick
        v' = velocityStep vessel v h t (f h)
        (_,vy') = v'
        sy = 0.5*constKSPPhysicsTick*abs (vy'-vy) + constKSPPhysicsTick * min vy' vy

-- | Given the parameters, return the list of tuples (Time,Velocity,Altitude)
-- until the vessel reaches space, reaches apoapsis, or runs out of fuel,
-- whichever comes first.
-- Includes a gravity kick and does not require initial conditions
-- (as they are taken from vessel data).
flyFromStart :: Vessel -> (Altitude -> Density) -> [(Time,Velocity,Altitude)]
flyFromStart vessel f =
  let
    launchBurn =
      takeWhile
      (\(time,vel,alt) -> magV vel
        < gravityKickSpeed vessel
        + constKSPPhysicsTick * currentAcceleration time alt)
      (fly initialConditions vessel f)
    (t,v,h) = last launchBurn
    v' = mulMatrixVector (rotMatrix (-gravityKickAngle vessel)) v
    gravityTurn = fly (t,v',h) vessel f
  in init launchBurn ++ gravityTurn
  where
    planet = currentPlanet vessel
    initialConditions = (0,(0,0),launchAltitude vessel)
    m0 = startingMass vessel
    thrust = engineForce vessel
    vEx = exhaustVelocity vessel
    g = gFieldStrength planet
    currentAcceleration t h = (thrust / (m0 - thrust * t / vEx)) - g h


--------------------
-- Quick vector math functions
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

mulMatrixVector :: Matrix2x2 -> Vector -> Vector
mulMatrixVector (c1,c2) (a,b) = mulSV a c1 `addV` mulSV b c2

rotMatrix :: Double -> Matrix2x2
rotMatrix angle = ((cos angle,sin angle),(-sin angle,cos angle))