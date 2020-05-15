{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( exports,
  )
where

import Godot
import NSClass.Hud2
import NSClass.MainScript2
import NSClass.Mob2
import NSClass.Player2

exports :: GdnativeHandle -> IO ()
exports desc = do
  registerClass $ RegClass desc $ classInit @Mob2
  registerClass $ RegClass desc $ classInit @Hud2
  registerClass $ RegClass desc $ classInit @Player2
  registerClass $ RegClass desc $ classInit @MainScript2
