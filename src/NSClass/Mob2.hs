{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module NSClass.Mob2
  ( Mob2 (..),
  )
where

import Godot
import System.Random (randomRIO)
import Util

mobTypes :: [Text]
mobTypes = ["walk", "swim", "fly"]

mobSpeedMin, mobSpeedMax :: Float
(mobSpeedMin, mobSpeedMax) = (150, 250)

getRandomMobType :: IO Text
getRandomMobType = do
  r <- randomRIO (0, (length mobTypes) - 1) :: IO Int
  return $ mobTypes !! r

getRandomMobSpeed :: IO Float
getRandomMobSpeed = randomRIO (mobSpeedMin, mobSpeedMax)

data Mob2
  = Mob2
      { _mob2_Base :: GodotRigidBody2D,
        _mob2_MobType :: Text,
        _mob2_Speed :: Float
      }

instance HasBaseClass Mob2 where
  type BaseClass Mob2 = GodotRigidBody2D
  super = _mob2_Base

instance NativeScript Mob2 where
  classInit base = Mob2 base <$> getRandomMobType <*> getRandomMobSpeed
  classMethods =
    [ func NoRPC "_ready" $
        \s _ -> do
          animated <- _get_child' s "AnimatedSprite" :: IO GodotAnimatedSprite
          toLowLevel (_mob2_MobType s) >>= set_animation animated,
      func NoRPC "_on_Visibility_screen_exited" $
        \s _ -> queue_free s,
      func NoRPC "_on_start_game" $
        \s _ -> queue_free s
    ]
