{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Tank
  ( initGame
  , Game(..)
  , height, width
  ) where

import Linear.V2 (V2(..), _x, _y)
import qualified Data.Sequence as S

-- Types

data Game = Game
  { _tank   :: Coord        -- ^ location of the tank

  } deriving (Show)

type Coord = V2 Int

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        {  _tank   = V2 xm ym
        }
  return $ execState _ g


-- -- Constants

height, width :: Int
height = 20
width = 20

-- Functions