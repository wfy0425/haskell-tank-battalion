{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game (
    Game(..)
    , tank, enemy, walls, stones, bullets
    , isGameLost, isGameOver, isGameWon, moveEnemy, moveTank, fire, step
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad (guard)
import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Linear.V2 (V2(..), _x, _y)

import Tank
import Global
import Bullet
import Hitable

-- types


data Game = Game
  { 
    _tank   :: Tank        -- ^ obj of the tank
  , _enemy  :: Tank       -- ^ obj of the enemy
  , _walls  :: [Wall]       -- ^ location of the walls
  , _stones :: [Stone]      -- ^ location of the stones
  , _bullets :: [Bullet]      -- ^ obj of the bullets
  } deriving (Show)


makeLenses ''Game

-- Functions

-- | update coordinate
-- | Bool: should run inside of the board
moveCoord :: Direction -> Bool -> Coord -> Coord
moveCoord d b c = case d of
    North    -> c & _y %~ (\y -> if b && y == height - 1 then y else y + 1)
    South  -> c & _y %~ (\y -> if b && y == 0 then y else y - 1)
    West  -> c & _x %~ (\x -> if b && x == 0 then x else x - 1)
    East -> c & _x %~ (\x -> if b && x == width - 1 then x else x + 1)

moveTank :: Direction -> Game -> Game
moveTank d g = g & tank . tankCoord %~ moveCoord d True 


moveEnemy :: Direction -> Game -> Game
moveEnemy d g = g & enemy . tankCoord %~ moveCoord d True

isGameOver :: Game -> Bool
isGameOver g = g ^. tank . tankHealth <= 0 || g ^. enemy . tankHealth <= 0

isGameWon :: Game -> Bool
isGameWon g = g ^. enemy . tankHealth <= 0

isGameLost :: Game -> Bool
isGameLost g = g ^. tank . tankHealth <= 0

-- | step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do 
    -- MaybeT $ guard . isGameOver $ s
    -- die <|> MaybeT (Just <$> modify bulletsFly) 
    MaybeT (Just <$> modify bulletsFly) 


die :: MaybeT (State Game) ()
die = error "to fill"


-- | Get next position of bullets
bulletsFly :: Game -> Game
bulletsFly g = g & bullets %~ map moveBullet 
-- bulletsFly g = g & bullets mapped %~ moveBullet 

moveBullet :: Bullet -> Bullet
moveBullet bullet@Bullet {_bulletDirection = bDir} = bullet & bulletCoord %~ moveCoord bDir False

fire :: Role -> Game -> Game 
fire SelfRole g@Game { _bullets = bs, _tank = t} = g & bullets .~ newBullet 
                                                        where
                                                            bulletCoord = moveCoord (t ^. tankDirection) False (t ^. tankCoord)
                                                            bulletDir = (t ^. tankDirection)
                                                            newBullet = (initBullet bulletCoord bulletDir : bs)
fire EnemyRole g@Game { _bullets = bs, _enemy = e} = g & bullets .~ newBullet 
                                                        where
                                                            bulletCoord = moveCoord (e ^. tankDirection) False (e ^. tankCoord)
                                                            bulletDir = (e ^. tankDirection)
                                                            newBullet = (initBullet bulletCoord bulletDir : bs)