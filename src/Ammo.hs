{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Ammo (
    Ammo(..)
    ,ammoCoord, ammoList, ammoIndex, ammoIncrease
    ,initAmmo
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

initAmmo :: Coord -> Int -> Ammo
initAmmo c x = Ammo {
            _ammoCoord = c
              , _ammoList = if x == 0 then[(V2 6 14), (V2 10 3), (V2 12 19), (V2 7 0), (V2 13 15), (V2 6 6)]
                                      else [(V2 3 5), (V2 18 13), (V2 6 4), (V2 11 15), (V2 19 19), (V2 0 0)]
              , _ammoIndex = 0
              , _ammoIncrease = 5
            }

data Ammo = Ammo {
    _ammoCoord :: Coord
    , _ammoList :: [Coord]
    , _ammoIndex :: Int
    , _ammoIncrease :: Int
} deriving (Show)

makeLenses ''Ammo