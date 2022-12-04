{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Tank
  ( Game(..)
  , Direction(..)
  , initTank
  , height, width, tank, moveTank, moveEnemy, tankCoord, enemy, 
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

import Global


-- Types
-- todo: move Game model into seperate file?
data Game = Game
  { 
    _tank   :: Tank        -- ^ obj of the tank
  , _enemy  :: Tank       -- ^ obj of the enemy
  , _walls  :: [Wall]       -- ^ location of the walls
  , _bullets :: [Bullet]      -- ^ obj of the bullets
  } deriving (Show)

type Coord = V2 Int

initTank :: Tank
initTank = Tank {
            _tankCoord = V2 xm ym
              , _tankDirection = North
              , _tankHealth = 100
            } where
                xm = width `div` 2
                ym = height `div` 2


data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)


data Tank = Tank {
  _tankCoord :: Coord
  , _tankDirection :: Direction
  , _tankHealth :: Int
} deriving (Show)


type Wall = Coord

data Bullet = Bullet {
  _bulletCoord :: Coord
  , _bulletDirection :: Direction
} deriving (Show)

makeLenses ''Game
makeLenses ''Tank
makeLenses ''Bullet


-- Functions


-- TODO: not working
moveTank :: Direction -> Game -> Game
moveTank d g = g & tank . tankCoord %~ moveCoord d
  where
    moveCoord :: Direction -> Coord -> Coord
    moveCoord d c = case d of
      North    -> c & _y %~ (\y -> if y == 0 then height - 1 else y - 1)
      South  -> c & _y %~ (\y -> if y == height - 1 then 0 else y + 1)
      West  -> c & _x %~ (\x -> if x == 0 then width - 1 else x - 1)
      East -> c & _x %~ (\x -> if x == width - 1 then 0 else x + 1)

-- TODO: not working
moveEnemy :: Direction -> Game -> Game
moveEnemy d g = g & enemy . tankCoord %~ moveCoord d
  where
    moveCoord :: Direction -> Coord -> Coord
    moveCoord d c = case d of
      North    -> c & _y %~ (\y -> if y == 0 then height - 1 else y - 1)
      South  -> c & _y %~ (\y -> if y == height - 1 then 0 else y + 1)
      West  -> c & _x %~ (\x -> if x == 0 then width - 1 else x - 1)
      East -> c & _x %~ (\x -> if x == width - 1 then 0 else x + 1)
