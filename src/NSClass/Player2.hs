{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module NSClass.Player2
  ( Player2 (..),
    player2_start,
  )
where

import Control.Lens
import Control.Monad
import Data.Function ((&))
import qualified Data.Text as T
import Foreign.C.String (withCString)
import Godot hiding (_process)
import qualified Godot.Gdnative.Internal.Api as Api
import Linear
import Util

_process :: Player2 -> [GodotVariant] -> IO ()
_process s [deltaVt] = do
  delta <- fromGodotVariant deltaVt :: IO Float
  screenSize <- readTVarIO (_player2_ScreenSize s)
  position <- get_position s >>= fromGVec2
  gInput <- Api.godot_global_get_singleton & withCString (T.unpack "Input") >>= _tryCast' :: IO GodotInput
  uiList <- mapM toLowLevel ["ui_right", "ui_left", "ui_down", "ui_up"] :: IO [GodotString]
  [pRight, pLeft, pDown, pUp] <- mapM (is_action_pressed gInput) uiList
  let speed = _player2_Speed s
      vec = V2 (getI pRight pLeft) (getI pDown pUp)
      nVec = normalize vec ^* (realToFrac speed)
      pVec =
        position
          & _x %~ ((+) $ (nVec & _x %~ (* delta)) ^. _x)
          & _y %~ ((+) $ (nVec & _y %~ (* delta)) ^. _y)
      cVec =
        pVec
          & _x %~ (clamp (screenSize ^. _x))
          & _y %~ (clamp (screenSize ^. _y))
  toGVec2 cVec >>= set_position s
  animatedSprite <- _get_child' s "AnimatedSprite" :: IO GodotAnimatedSprite
  case vec of
    V2 0 0 -> stop animatedSprite
    _ -> toLowLevel "" >>= play animatedSprite
  case vec of
    _
      | (vec ^. _x) /= 0 -> do
        toLowLevel "right" >>= set_animation animatedSprite
        set_flip_v animatedSprite False
        set_flip_h animatedSprite $ (vec ^. _x) < 0
      | (vec ^. _y) /= 0 -> do
        toLowLevel "up" >>= set_animation animatedSprite
        set_flip_h animatedSprite $ (vec ^. _y) > 0
      | otherwise -> return ()
  where
    getI :: Bool -> Bool -> Float
    getI True False = 1
    getI False True = -1
    getI _ _ = 0
    clamp mx i = max 0 $ min mx i
_process _ _ = return ()

player2_start :: Player2 -> GodotVector2 -> IO ()
player2_start s position = do
  set_position s position
  Godot.show s
  collisionShape2D <- _get_child' s "CollisionShape2D" :: IO GodotCollisionShape2D
  set_disabled collisionShape2D False

data Player2
  = Player2
      { _player2_Base :: GodotArea2D,
        _player2_Speed :: Int,
        _player2_ScreenSize :: TVar (V2 Float)
      }

instance HasBaseClass Player2 where
  type BaseClass Player2 = GodotArea2D
  super = _player2_Base

instance NativeScript Player2 where
  classInit base = Player2 base 400 <$> atomically (newTVar (error "Failed to initialize Player2"))
  classMethods =
    [ func NoRPC "_ready" $
        \s _ -> do
          rect <- get_viewport_rect s
          rectSize <- Api.godot_rect2_get_size rect >>= fromGVec2
          atomically $ writeTVar (_player2_ScreenSize s) rectSize
          hide s,
      func NoRPC "_on_Player_body_entered" $
        \s _ -> do
          hide s
          hitStr <- toLowLevel "hit" :: IO GodotString
          void $ emit_signal s hitStr []
          collisionShape2D <- _get_child' s "CollisionShape2D" :: IO GodotCollisionShape2D
          disabledStr <- toLowLevel "disabled" :: IO GodotString
          trueVariant <- toLowLevel (toVariant True)
          set_deferred collisionShape2D disabledStr trueVariant,
      func NoRPC "start" $
        \s d -> case d of
          [posVt] -> do
            pos <- fromGodotVariant posVt
            player2_start s pos
          _ -> return (),
      func NoRPC "_process" _process
    ]
  classSignals =
    [ signal "hit" []
    ]
