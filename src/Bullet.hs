{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Bullet (
    Bullet(..)
) where

import Global
import Control.Lens hiding ((<|), (|>), (:>), (:<))


data Bullet = Bullet {
  _bulletCoord :: Coord -- bullet origin
  , _bulletDirection :: Direction
} deriving (Show)

makeLenses ''Bullet

-- todo: bulletFly
-- todo: hit
-- todo: fire (role?)
-- todo: 这车得要有厚度啊，不然和bullet一个样子了
-- todo: 音效