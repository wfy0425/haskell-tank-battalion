{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Bullet (
    Bullet(..)
) where

import Global
import Control.Lens hiding ((<|), (|>), (:>), (:<))


data Bullet = Bullet {
  _bulletCoord :: Coord
  , _bulletDirection :: Direction
} deriving (Show)

makeLenses ''Bullet