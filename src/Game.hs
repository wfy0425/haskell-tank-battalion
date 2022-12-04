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


moveCoord :: Direction -> Coord -> Coord
moveCoord d c = case d of
    North    -> c & _y %~ (\y -> if y == height - 1 then y else y + 1)
    South  -> c & _y %~ (\y -> if y == 0 then y else y - 1)
    West  -> c & _x %~ (\x -> if x == 0 then x else x - 1)
    East -> c & _x %~ (\x -> if x == width - 1 then x else x + 1)

moveTank :: Direction -> Game -> Game
moveTank d g = g & tank . tankCoord %~ moveCoord d  


moveEnemy :: Direction -> Game -> Game
moveEnemy d g = g & enemy . tankCoord %~ moveCoord d

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
moveBullet bullet@Bullet {_bulletDirection = bDir} = bullet & bulletCoord %~ moveCoord bDir

fire :: Role -> Game -> Game 
fire SelfRole g@Game { _bullets = bs, _tank = t} = g & bullets .~ newBullet 
                                                        where
                                                            bulletCoord = moveCoord (t ^. tankDirection) (t ^. tankCoord)
                                                            bulletDir = (t ^. tankDirection)
                                                            newBullet = (initBullet bulletCoord bulletDir : bs)
fire EnemyRole g@Game { _bullets = bs, _enemy = e} = g & bullets .~ newBullet 
                                                        where
                                                            bulletCoord = moveCoord (e ^. tankDirection) (e ^. tankCoord)
                                                            bulletDir = (e ^. tankDirection)
                                                            newBullet = (initBullet bulletCoord bulletDir : bs)