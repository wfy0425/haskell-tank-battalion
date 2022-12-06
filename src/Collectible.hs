{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Collectible 
    ( Collectible(..)
    , initCollectible, collectibleCoord, coordinateList, coordinateIndex, health
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



initCollectible :: Int -> Int -> Collectible
initCollectible xm ym = Collectible {
            _collectibleCoord = V2 xm ym
            , _coordinateList = [(V2 3 5), (V2 18 13), (V2 6 4), (V2 11 15), (V2 19 19), (V2 0 0)]
            , _coordinateIndex = 0
            , _health = 20
            }

data Collectible = Collectible {
    _collectibleCoord :: Coord
    , _coordinateList :: [Coord]
    , _coordinateIndex :: Int
    , _health :: Int
} deriving (Show)

makeLenses ''Collectible

