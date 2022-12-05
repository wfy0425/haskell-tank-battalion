{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game (
    Game(..)
    , tank, enemy, walls, stones, bullets, selfBase, enemyBase
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
  , _selfBase :: Base
  , _enemyBase :: Base
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

nextCoord :: Direction -> Coord -> Coord
nextCoord d c = do
    let x = c ^. _x
    let y = c ^. _y
    case d of
      North -> V2 x (y + 1)
      South -> V2 x (y - 1)
      West -> V2 (x - 1) y
      East -> V2 (x + 1) y 

moveTank :: Direction -> Game -> Game
moveTank d g = do
  let c = nextCoord d (g ^. tank . tankCoord)
  let x = c ^. _x
  let y = c ^. _y
  if (x >= 0 && x < width && y >= 0 && y < height && (c `notElem` g ^. walls) && (c `notElem` g ^. stones)) then
    g & tank . tankCoord .~ c & tank . tankDirection .~ d
  else
    g & tank . tankDirection .~ d
  
moveEnemy :: Direction -> Game -> Game
moveEnemy d g = do
  let c = nextCoord d (g ^. enemy . tankCoord)
  let x = c ^. _x
  let y = c ^. _y
  if (x >= 0 && x < width && y >= 0 && y < height && (c `notElem` g ^. walls) && (c `notElem` g ^. stones)) then
    g & enemy . tankCoord .~ c & enemy . tankDirection .~ d
  else
    g & enemy . tankDirection .~ d

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
    hit <|> attack <|> MaybeT (Just <$> modify bulletsFly) 


die :: MaybeT (State Game) ()
die = error "to fill"

-- bulletCoords :: Game -> [Coord] 
-- bulletCoords g = [ c | c <- g ^. bullets ]

hit :: MaybeT (State Game) ()
hit = do
    bulletGetter <- use bullets
    wallGetter <- use walls
    stoneGetter <- use stones
    let coordsToBeDel = [ b ^. bulletCoord | b <- bulletGetter, 
                                             w <- wallGetter, 
                                             s <- stoneGetter, 
                                             b ^. bulletCoord  == w || b ^. bulletCoord == s ]

    guard $ not $ null coordsToBeDel
    MaybeT . fmap Just $ do
        modifying bullets (delBullets coordsToBeDel)
        modifying walls (delWalls coordsToBeDel)

delBullets :: [Coord] -> [Bullet] -> [Bullet]
delBullets coordsToBeDel bs = filter (\b -> not ((b ^. bulletCoord) `elem` coordsToBeDel)) bs

delWalls :: [Coord] -> [Coord] -> [Coord]
delWalls coordsToBeDel ws = filter (\w -> not ( w `elem` coordsToBeDel) ) ws

attack :: MaybeT (State Game) ()
attack = do 
        bulletGetter <- use bullets
        tankGetter <- use tank
        enemyGetter <- use enemy
        let coordsToBeDel = [ b ^. bulletCoord | b <- bulletGetter,
                                                b ^. bulletCoord  == tankGetter ^. tankCoord 
                                                || b ^. bulletCoord == enemyGetter ^. tankCoord ]
        guard $ not $ null coordsToBeDel
        MaybeT . fmap Just $ do
            modifying tank (hurt coordsToBeDel)
            modifying enemy (hurt coordsToBeDel)
            modifying bullets (delBullets coordsToBeDel)

hurt :: [Coord] -> Tank -> Tank
hurt cs t = if (t ^. tankCoord `elem` cs && t ^. tankHealth > 0) then t & tankHealth .~ (t ^. tankHealth - 10) else t

-- | Get next position of bullets
bulletsFly :: Game -> Game
bulletsFly g = g & bullets %~ map moveBullet 

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