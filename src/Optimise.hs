module Optimise (findBestLaunch) where

import Data.List (sortOn)
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Lib (Altitude, Density, Vessel (..), altitudeAt, atmosphereHeight, deltaVToOrbit, findApsis, magV)
import Simulation (flyFromStart)

-- | Find the best launch trajectory by bruteforcing possible trajectories.
-- Returns the given trajectory if a trajectory that has an apsis outside
-- the atmosphere could not be found.
findBestLaunch :: (Altitude -> Density) -> Vessel -> Vessel
-- TODO: optimise the algorithm somewhat, right now it takes about a minute to run
findBestLaunch f vessel =
  let vessels = [vessel {gravityKickAngle = angle, gravityKickSpeed = speed} | angle <- [0, 0.00175 .. 0.525], speed <- [20, 40 .. 160]]
      trajectories = [(v, last trajectory) | v <- vessels, let trajectory = flyFromStart v f, not (null trajectory)]
      apsides = [(v, fromJust apsis) | (v, trajectory) <- trajectories, let apsis = findApsis planet trajectory, isJust apsis]
      apsides' = filter (\(_, apsis) -> inSpace apsis) apsides
      bestVessel = listToMaybe . map fst $ sortOn (dv . snd) apsides'
   in fromMaybe vessel bestVessel
  where
    planet = currentPlanet vessel
    inSpace (_, _, pos) = altitudeAt planet pos >= atmosphereHeight planet
    dv (_, v, pos) = deltaVToOrbit planet (altitudeAt planet pos) (magV v)
