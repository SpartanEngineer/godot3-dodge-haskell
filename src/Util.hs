{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Util where

import Control.Lens
import Control.Monad
import Data.Function ((&))
import Data.Text as T
import Foreign.C (withCString)
import Foreign.C.Types (CFloat (..))
import Godot
import Godot.Gdnative.Internal
import Godot.Gdnative.Internal.Api as Api
import Godot.Gdnative.Types
import Godot.Methods (get_class, instance')
import qualified Godot.Methods as G
import Linear
import Unsafe.Coerce (unsafeCoerce)

godotPrint :: String -> IO ()
godotPrint str = godot_print `submerge` (T.pack str)

godotTPrint :: Text -> IO ()
godotTPrint txt = godot_print `submerge` txt

-- isClass :: GodotObject :< a => a -> Text -> IO Bool
-- isClass obj clsName = do
--   objClass <- G.get_class (GodotNode $ tryCast obj) >>= fromLowLevel
--   let clsName' =
--         if objClass /= "" && T.head objClass == '_'
--           then "_" `T.append` clsName
--           else clsName
--   ((toLowLevel clsName') :: IO GodotString) >>= G.is_class ((tryCast obj) :: GodotObject)

-- asClass :: (GodotObject :< a, a :< b)
--        => (GodotObject -> b)
--        -> Text
--        -> a
--        -> IO (Maybe b)
-- asClass constr clsName a = do
--   isClass' <- a `isClass` clsName
--   return $ if isClass' then Just $ constr $ tryCast a else Nothing

-- asClass' :: (GodotObject :< a, a :< b) => (GodotObject -> b) -> Text -> a -> IO b
-- asClass' constr clsName a = asClass constr clsName a >>= \case
--   Just a' -> return a'
--   Nothing -> error $ "Could not cast to " ++ (T.unpack clsName)

getClassDB :: IO Godot_ClassDB
getClassDB = do
  db <- Api.godot_global_get_singleton & withCString (unpack "ClassDB") >>= tryCast
  case db of
    Just a -> return a
    Nothing -> error "Unable to get ClassDB"

mkClassInstance :: Text -> IO GodotObject
mkClassInstance cName = do
  classDB <- getClassDB
  cls <-
    instance' classDB `submerge` cName
      >>= fromGodotVariant
  get_class cls
    >>= fromLowLevel
    >>= godotPrint . mappend "I made a " . T.unpack
  return cls

getSingleton :: Text -> IO GodotObject
getSingleton name =
  godot_global_get_singleton
    & withCString (T.unpack name)

-- Type conversions

dip ::
  (GodotFFI low high, GodotFFI low' high') =>
  (low -> IO low') ->
  high ->
  IO high'
dip lowf high = lowf `submerge` high >>= fromLowLevel

submerge :: GodotFFI low high => (low -> IO a) -> high -> IO a
submerge lowf high = toLowLevel high >>= lowf

withVariant f v = fromGodotVariant v >>= f

load :: Text -> Text -> IO (Maybe GodotResource)
load clsName url = do
  godotTPrint $ "Loading: " <> url
  rlMaybe <- getSingleton "ResourceLoader" >>= tryCast :: IO (Maybe Godot_ResourceLoader)
  rl <- case rlMaybe of
    Just r -> return r
    Nothing -> error "Unable to load global ResourceLoader"
  url' <- toLowLevel url
  clsName' <- toLowLevel clsName
  res <- G.exists rl url' clsName' >>= \exist ->
    if exist
      then Just <$> G.load rl url' clsName' False
      else return Nothing
  return res

load' :: Text -> Text -> IO GodotResource
load' clsName url = Util.load clsName url >>= \case
  Just res -> return $ res
  Nothing -> error $ T.unpack $ "load' unable to load " <> clsName <> " at: " <> url

newNS :: [Variant 'GodotTy] -> Text -> IO (Maybe GodotObject)
newNS args url = do
  resourceMaybe <- Util.load "NativeScript" url :: IO (Maybe GodotResource)
  let objMaybe = case resourceMaybe of
        Just n -> Just (safeCast n :: GodotObject)
        Nothing -> Nothing
  case objMaybe of
    Just ns -> Just <$> G.new ((unsafeCoerce ns) :: GodotNativeScript) args
    Nothing -> return Nothing

newNS' :: [Variant 'GodotTy] -> Text -> IO GodotObject
newNS' args url = newNS args url >>= \case
  Just ns -> return ns
  Nothing -> error "newNS' unable to make new NativeScript object"

-- random utility functions

_getGodotOS :: IO Godot_OS
_getGodotOS = Api.godot_global_get_singleton & withCString (T.unpack "OS") >>= _tryCast'

_tryCast' self = do
  res <- tryCast self
  case res of
    Just some -> return some
    Nothing -> error $ "Unsafe cast error"

_get_child' node name = do
  arr <- get_children node
  arrVt <- fromLowLevel' arr
  nodes <- mapM fromGodotVariant arrVt
  res <- filterM isName nodes
  case res of
    [] -> error $ T.unpack $ "Unable to find child: " <> name
    x : (_) -> return x
  where
    fromLowLevel' vs = do
      sz <- fromIntegral <$> Api.godot_array_size vs
      forM [0 .. sz -1] $ Api.godot_array_get vs
    isName x = do
      xNameLow <- get_name x :: IO GodotString
      high <- fromLowLevel xNameLow :: IO Text
      return (high == name)

_get_node node name = get_node node `submerge` name

_get_node' node name = get_node node `submerge` name >>= _tryCast'

_asNativeScript :: NativeScript b => GodotObject -> IO b
_asNativeScript obj = asNativeScript obj >>= \case
  Just some -> return some
  Nothing -> error $ "_asNativeScript unable to cast obj to native script"

fromGVec2 :: GodotVector2 -> IO (V2 Float)
-- I believe you can also do this via to/fromLowLevel
fromGVec2 gvec = do
  x <- Api.godot_vector2_get_x gvec
  y <- Api.godot_vector2_get_y gvec
  return $ realToFrac <$> V2 x y

toGVec2 :: V2 Float -> IO GodotVector2
-- I believe you can also do this via to/fromLowLevel
toGVec2 vec2 = Api.godot_vector2_new (CFloat (vec2 ^. _x)) (CFloat (vec2 ^. _y))
