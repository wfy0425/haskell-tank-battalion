{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Tank where

import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Global
import Linear.V2 (V2 (..), _x, _y)
import System.Random (Random (..), newStdGen)

-- Types

initTank :: Coord -> Tank
initTank c =
  Tank
    { _tankCoord = c,
      _tankDirection = North,
      _tankHealth = 100,
      _baseHealth = 200,
      _tankBlinkCount = 0,
      _damageTaken = 10
    }

data Tank = Tank
  { _tankCoord :: Coord,
    _tankDirection :: Direction,
    _tankHealth :: Int,
    _baseHealth :: Int,
    -- | used to blink the tank when it is hit
    _tankBlinkCount :: Int,
    _damageTaken :: Int
  }
  deriving (Show, Eq)

makeLenses ''Tank
