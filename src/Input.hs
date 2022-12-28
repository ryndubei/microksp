module Input (handleKeys, update) where

import Lib
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S
import Graphics.Gloss.Geometry.Angle (degToRad)

handleKeys :: Event -> Vessel -> Vessel
handleKeys (EventKey (Char 'z') Down _ _) vessel =
  if imageScale vessel <= 1
    then vessel
    else vessel { imageScale = imageScale vessel `div` 2}
handleKeys (EventKey (Char 'x') Down _ _) vessel = vessel { imageScale = imageScale vessel * 2}
handleKeys (EventKey (Char 'p') Down _ _) vessel = 
  vessel { currentPlanet = if currentPlanet vessel /= maxBound 
                            then succ (currentPlanet vessel)
                            else minBound }
handleKeys (EventKey k Down _ _) vessel = 
  if isMovementKey k
    then vessel {keys = S.insert k (keys vessel)}
    else vessel
handleKeys (EventKey k Up _ _) vessel = vessel {keys = S.delete k (keys vessel)}
handleKeys _ vessel = vessel

-- | lshift/lctrl : increase/decrease thrust, left/right : change gravity kick angle,
-- up/down : increase/decrease gravity turn start velocity
isMovementKey :: Key -> Bool
isMovementKey = flip S.member movementKeys
  where
    movementKeys :: S.Set Key
    movementKeys =
      S.fromList
      [SpecialKey KeyUp, SpecialKey KeyDown
      , SpecialKey KeyRight, SpecialKey KeyLeft
      , SpecialKey KeyShiftL, SpecialKey KeyCtrlL]

update :: Float -> Vessel -> Vessel
update _ = handleMove

handleMove :: Vessel -> Vessel
handleMove vessel = S.foldr moveKeys vessel (keys vessel)
  where
    moveKeys :: Key -> Vessel -> Vessel
    moveKeys (SpecialKey k) vessel' =
      case k of
        KeyUp -> vessel' { gravityKickSpeed = gravityKickSpeed vessel' + 0.5 }
        KeyDown -> if gravityKickSpeed vessel' - 0.5 >= 0
          then vessel' { gravityKickSpeed = gravityKickSpeed vessel' - 0.5}
          else vessel'
        KeyShiftL -> vessel' { engineForce = 1.01 * engineForce vessel' }
        -- prevent engine from being weaker than 1g at 0m
        KeyCtrlL -> if 0.99 * engineForce vessel' >= gFieldStrength (currentPlanet vessel') 0 * startingMass vessel'
          then vessel' { engineForce = 0.99 * engineForce vessel' }
          else vessel'
        -- prevent gravity kick from being lower than 0 degrees (i.e always right)
        KeyLeft -> if gravityKickAngle vessel' - drd 0.1 >= 0
          then vessel' { gravityKickAngle = gravityKickAngle vessel' - drd 0.1 }
          else vessel'
        -- prevent gravity kick from being higher than 90 degrees (i.e always above horizon)
        KeyRight -> if gravityKickAngle vessel' + drd 0.1 <= drd 90
          then vessel' { gravityKickAngle = gravityKickAngle vessel' + drd 0.1 }
          else vessel'
        _ -> vessel'
    moveKeys _ vessel' = vessel'
    drd = realToFrac . degToRad