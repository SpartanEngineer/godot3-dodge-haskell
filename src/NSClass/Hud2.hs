{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module NSClass.Hud2
  ( Hud2 (..),
    hud2_show_message,
    hud2_update_score,
    hud2_show_game_over,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import qualified Data.Text as T
import Godot
import Util

data Hud2
  = Hud2
      { _hud2_Base :: GodotCanvasLayer
      }

instance HasBaseClass Hud2 where
  type BaseClass Hud2 = GodotCanvasLayer
  super = _hud2_Base

hud2_show_message :: Hud2 -> Text -> IO ()
hud2_show_message s message = do
  messageLabel <- _get_child' s "MessageLabel" :: IO GodotLabel
  toLowLevel message >>= set_text messageLabel
  Godot.show messageLabel
  messageTimer <- _get_child' s "MessageTimer" :: IO GodotTimer
  start messageTimer (-1)

hud2_update_score :: Hud2 -> Int -> IO ()
hud2_update_score s score = do
  scoreLabel <- _get_node' s "ScoreLabel" :: IO GodotLabel
  set_text scoreLabel `submerge` (T.pack $ Prelude.show score)

hud2_show_game_over :: Hud2 -> IO ()
hud2_show_game_over s = void $ async $ do
  hud2_show_message s "Game Over"
  threadDelay 1000000 --sleep for a million microseconds, or one second
  messageLabel <- _get_child' s "MessageLabel" :: IO GodotLabel
  toLowLevel ("Dodge the \nCreeps!" :: Text) >>= set_text messageLabel
  threadDelay 1000000 --sleep for a million microseconds, or one second
  startButton <- _get_child' s "StartButton" :: IO GodotButton
  Godot.show startButton

instance NativeScript Hud2 where
  classInit base = pure $ Hud2 base
  classMethods =
    [ func NoRPC "show_message" $
        \s d -> case d of
          [messageVt] -> do
            message <- fromGodotVariant messageVt :: IO GodotString
            fromLowLevel message >>= hud2_show_message s
          _ -> return (),
      func NoRPC "show_game_over" $
        \s _ -> hud2_show_game_over s,
      func NoRPC "update_score" $
        \s d -> case d of
          [scoreVt] -> do
            score <- fromGodotVariant scoreVt :: IO Int
            hud2_update_score s score
          _ -> return (),
      func NoRPC "_on_StartButton_pressed" $
        \s _ -> do
          startButton <- _get_child' s "StartButton" :: IO GodotLabel
          hide startButton
          gameStr <- toLowLevel "start_game" :: IO GodotString
          void $ emit_signal s gameStr [],
      func NoRPC "_on_MessageTimer_timeout" $
        \s _ -> do
          messageLabel <- _get_child' s "MessageLabel" :: IO GodotLabel
          hide messageLabel
    ]
  classSignals =
    [ signal "start_game" []
    ]
