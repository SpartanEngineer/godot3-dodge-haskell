{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module NSClass.MainScript2
  ( MainScript2 (..),
  )
where

import Control.Monad
import Foreign.C.Types (CFloat (..))
import Godot
import qualified Godot.Gdnative.Internal.Api as Api
import Linear
import NSClass.Hud2
import NSClass.Mob2
import NSClass.Player2
import Util

data MainScript2
  = MainScript2
      { _mainScript2_Base :: GodotNode2D,
        _mainScript2_Score :: TVar Int,
        _mainScript2_rng :: TVar GodotRandomNumberGenerator
      }

instance HasBaseClass MainScript2 where
  type BaseClass MainScript2 = GodotNode2D
  super = _mainScript2_Base

instance NativeScript MainScript2 where
  classInit base = MainScript2 base <$> atomically (newTVar 0) <*> atomically (newTVar (error "Failed to initialize MainScript2"))
  classMethods =
    [ func NoRPC "_ready" $
        \s _ -> do
          rng <- mkClassInstance "RandomNumberGenerator" >>= _tryCast' :: IO GodotRandomNumberGenerator
          randomize rng
          atomically $ writeTVar (_mainScript2_rng s) rng,
      func NoRPC "game_over" $
        \s _ -> do
          scoreTimer <- _get_child' s "ScoreTimer" :: IO GodotTimer
          stop scoreTimer
          mobTimer <- _get_child' s "MobTimer" :: IO GodotTimer
          stop mobTimer
          hud2Node <- _get_node s "HUD2"
          hud2 <- _asNativeScript (safeCast hud2Node) :: IO Hud2
          hud2_show_game_over hud2
          music <- _get_child' s "Music" :: IO GodotAudioStreamPlayer
          stop music
          deathSound <- _get_child' s "DeathSound" :: IO GodotAudioStreamPlayer
          play deathSound 0,
      func NoRPC "new_game" $
        \s _ -> do
          atomically $ writeTVar (_mainScript2_Score s) 0
          startPosition <- _get_node' s "StartPosition" :: IO GodotPosition2D
          position <- get_position startPosition
          player2Node <- _get_node s "Player2"
          player2 <- _asNativeScript (safeCast player2Node) :: IO Player2
          player2_start player2 position
          startTimer <- _get_child' s "StartTimer" :: IO GodotTimer
          start startTimer (-1)
          hud2Node <- _get_node s "HUD2"
          hud2 <- _asNativeScript (safeCast hud2Node) :: IO Hud2
          hud2_update_score hud2 0
          hud2_show_message hud2 "Get Ready"
          music <- _get_child' s "Music" :: IO GodotAudioStreamPlayer
          play music 0,
      func NoRPC "_on_StartTimer_timeout" $
        \s _ -> do
          mobTimer <- _get_child' s "MobTimer" :: IO GodotTimer
          start mobTimer (-1)
          scoreTimer <- _get_child' s "ScoreTimer" :: IO GodotTimer
          start scoreTimer (-1),
      func NoRPC "_on_ScoreTimer_timeout" $
        \s _ -> do
          score <- readTVarIO (_mainScript2_Score s)
          let score' = score + 1
          atomically $ writeTVar (_mainScript2_Score s) score'
          hud2Node <- _get_node s "HUD2"
          hud2 <- _asNativeScript (safeCast hud2Node) :: IO Hud2
          hud2_update_score hud2 score',
      func NoRPC "_on_MobTimer_timeout" $
        \s _ -> do
          rng <- readTVarIO (_mainScript2_rng s)
          rNum <- randi rng
          mobSpawnLocation <- _get_node' s "MobPath/MobSpawnLocation" :: IO GodotPathFollow2D
          set_offset mobSpawnLocation $ realToFrac rNum
          mobPackedScene <- load' "PackedScene" "res://Mob2.tscn" >>= _tryCast' :: IO GodotPackedScene
          mobObj <- instance' mobPackedScene 0
          mob2 <- _asNativeScript (safeCast mobObj) :: IO Mob2
          add_child s (safeCast mob2) False
          position <- get_position mobSpawnLocation
          set_position mob2 position
          originalRotation <- get_rotation mobSpawnLocation
          directionRandom <- randf_range rng (0 - (pi / 4)) (pi / 4)
          let mobRotation = originalRotation + (pi / 2) + directionRandom
          set_rotation mob2 mobRotation
          linVel <- toLowLevel $ V2 (_mob2_Speed mob2) 0.0
          linVelR <- Api.godot_vector2_rotated linVel (CFloat mobRotation)
          set_linear_velocity mob2 linVelR
          hud2Node <- _get_node s "HUD2"
          hud2 <- _asNativeScript (safeCast hud2Node) :: IO Hud2
          startGameStr <- toLowLevel "start_game" :: IO GodotString
          onStartGameStr <- toLowLevel "_on_start_game" :: IO GodotString
          gArr <- Api.godot_array_new
          void $ connect hud2 startGameStr (safeCast mob2) onStartGameStr gArr 0
    ]
