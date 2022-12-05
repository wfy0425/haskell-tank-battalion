{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Tank
  ( Tank(..)
  , tankCoord, tankHealth, tankDirection, baseHealth
  , initTank
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

initTank :: Int -> Int -> Tank
initTank xm ym = Tank {
            _tankCoord = V2 xm ym
              , _tankDirection = North
              , _tankHealth = 100
              , _baseHealth = 200
            } 


data Tank = Tank {
  _tankCoord :: Coord
  , _tankDirection :: Direction
  , _tankHealth :: Int
  , _baseHealth :: Int
} deriving (Show)


makeLenses ''Tank


