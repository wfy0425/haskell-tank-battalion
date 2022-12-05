{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Collectible 
    ( Collectible(..)
    , initCollectible, collectibleCoord, healthMod 
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
            , _healthMod = 20
            }

data Collectible = Collectible {
    _collectibleCoord :: Coord
    , _healthMod :: Int
} deriving (Show)

makeLenses ''Collectible

