{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Game where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import Control.Monad (guard)
import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Linear.V2 (V2(..), _x, _y)
import System.Random
import Tank
import Global
import Bullet
import Hitable
import Ammo
import Collectible
-- types

data StageData = StageData {
  _tankPos :: Coord
  ,_enemyPos :: Coord
  ,_wallsPos :: [Coord]
  ,_stonesPos :: [Coord]
  ,_lakesPos :: [Coord]
  ,_selfBasePos :: [Coord]
  ,_enemyBasePos :: [Coord]
  ,_ammoPos :: Coord
  ,_collectiblePos :: Coord
} deriving(Show, Read)
makeLenses ''StageData

data Game = Game
  {
    _tank   :: Tank        -- ^ obj of the tank
  , _enemy  :: Tank       -- ^ obj of the enemy
  , _walls  :: [Wall]       -- ^ location of the walls
  , _stones :: [Stone]      -- ^ location of the stones
  , _lakes :: [Lake]       -- ^ location of the lakes
  , _bullets :: [Bullet]      -- ^ obj of the bullets
  , _selfBase :: Base
  , _enemyBase :: Base
  , _gameOver :: Bool
  , _collectible :: Collectible
  , _gameState :: GameState
  , _ammo :: Ammo

  , _stageData :: [StageData]
  , _currentStageIdx :: Int 
  } deriving (Show)

makeLenses ''Game



-- Functions

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do 
  let stageData = [StageData {
      _tankPos = V2 (width - 3) 2 
      ,_enemyPos = V2 2 (height-3)
      ,_wallsPos = [
        V2 5 11,V2 5 12,V2 5 13,V2 5 14,V2 5 15,V2 5 16,V2 5 17,V2 5 18,
        V2 6 11,V2 6 18,
        V2 7 11,V2 7 18,
        V2 8 11,V2 8 18,
        V2 9 11,V2 9 18,
        V2 10 11,V2 10 18,
        V2 11 11,V2 11 18,
        V2 12 11,V2 12 18,
        V2 13 11,V2 13 18,
        V2 14 11,V2 14 18,
        V2 15 11,V2 15 12,V2 15 13,V2 15 14,V2 15 15,V2 15 16,V2 15 17,V2 15 18,
        V2 4 1,V2 4 2,V2 4 3,V2 4 4,V2 4 5,V2 4 6,V2 4 7,V2 4 8,
        V2 5 1,V2 5 8,
        V2 6 1,V2 6 8,
        V2 7 1,V2 7 8,
        V2 8 1,V2 8 8,
        V2 9 1,V2 9 8,
        V2 10 1,V2 10 8,
        V2 11 1,V2 11 8,
        V2 12 1,V2 12 8,
        V2 13 1,V2 13 8,
        V2 14 1,V2 14 2,V2 14 3,V2 14 4,V2 14 5,V2 14 6,V2 14 7,V2 14 8]
      ,_stonesPos = [
        V2 7 14,V2 7 15,V2 7 16,
        V2 8 14,V2 8 15,V2 8 16,
        V2 11 3,V2 11 4,V2 11 5,
        V2 12 3,V2 12 4,V2 12 5]
      ,_lakesPos = [
        V2 5 4,V2 5 5,V2 5 10,
        V2 4 10,V2 4 11,
        V2 10 4,V2 10 5,V2 10 10,
        V2 11 10]
      ,_selfBasePos = [V2 (width - 2) (height - 2), V2 (width - 2) (height - 3), V2 (width - 3) (height - 2), V2 (width - 3) (height - 3)]
      ,_enemyBasePos = [V2 1 1, V2 1 2, V2 2 1, V2 2 2]
     ,_ammoPos = V2 10 9
     ,_collectiblePos = V2 16 5
  }, StageData {
      _tankPos = V2 (width - 3) 2 
      ,_enemyPos = V2 2 (height-3)
      ,_wallsPos = [
        V2 5 19,V2 5 12,V2 5 13,V2 5 14,V2 5 15,V2 5 16,V2 5 17,V2 5 18,
        V2 6 19,V2 6 18, V2 6 13,V2 6 15,
        V2 7 19,V2 7 18, V2 7 13,V2 7 15,
        V2 8 19,V2 8 18, V2 8 13,V2 8 15,
        V2 9 19,V2 9 18, V2 9 13,V2 9 15,
        V2 10 19,V2 10 18, V2 10 13,V2 10 15,
        V2 11 19,V2 11 18, V2 11 13,V2 11 15,
        V2 12 19,V2 12 18, V2 12 13,V2 12 15,
        V2 13 19,V2 13 18, V2 13 13,V2 13 15,
        V2 14 19,V2 14 18, V2 14 13,V2 14 15,
        V2 15 19,V2 15 12,V2 15 13,V2 15 14,V2 15 15,V2 15 16,V2 15 17,V2 15 18
        ]
      ,_stonesPos = [V2 7 14]
      ,_lakesPos = [
        V2 4 4,V2 4 5,V2 4 6,V2 4 7,V2 4 8,
        V2 5 4,V2 5 5,V2 5 6,V2 5 7,V2 5 8,
        V2 6 4,V2 6 5,V2 6 6,V2 6 7,V2 6 8,
        V2 7 4,V2 7 5,V2 7 6,V2 7 7,V2 7 8,
        V2 8 4,V2 8 5,V2 8 6,V2 8 7,V2 8 8,
        V2 9 4,V2 9 5,V2 9 6,V2 9 7,V2 9 8,
        V2 10 4,V2 10 5,V2 10 6,V2 10 7,V2 10 8,
        V2 11 4,V2 11 5,V2 11 6,V2 11 7,V2 11 8,
        V2 12 4,V2 12 5,V2 12 6,V2 12 7,V2 12 8,
        V2 13 4,V2 13 5,V2 13 6,V2 13 7,V2 13 8,
        V2 14 4,V2 14 5,V2 14 6,V2 14 7,V2 14 8,
        V2 15 4,V2 15 5,V2 15 6,V2 15 7,V2 15 8
        ]
      ,_selfBasePos = [V2 (width - 2) (height - 2), V2 (width - 2) (height - 3), V2 (width - 3) (height - 2), V2 (width - 3) (height - 3)]
      ,_enemyBasePos = [V2 1 1, V2 1 2, V2 2 1, V2 2 2]
      ,_ammoPos = V2 10 9
      ,_collectiblePos = V2 16 5
  }]
  return $ initialWorld stageData

-- create world from initData and set state to GameReady
initialWorld :: [StageData] -> Game
initialWorld stageData = launchGame stageData 0 GameReady

-- | Select a new game with the given stageData
changeToIndexWorld :: Int -> Game -> Game
changeToIndexWorld idx g = launchGame (_stageData g) idx GameSelecting

-- common func: launch a new game with the specific stageData and gameState  
launchGame :: [StageData]  -> Int -> GameState -> Game
launchGame stageData index gameState = Game{
              _tank  = initTank (s ^. tankPos) 
              , _enemy = initTank (s ^. enemyPos)
              , _walls = map initWall (s ^. wallsPos)
              , _stones = map initStone (s ^. stonesPos)
              , _lakes = map initLake (s ^. lakesPos)
              , _bullets = []
              , _selfBase = s ^. selfBasePos
              , _enemyBase = s ^. enemyBasePos
              , _gameOver = False
              , _collectible = initCollectible (s ^. collectiblePos) index
              , _gameState = gameState
              , _ammo = initAmmo (s ^. ammoPos) index
              , _stageData = stageData
              , _currentStageIdx = index
              }
              where
                s = stageData !! index



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

moveTank :: Role -> Direction -> Game -> Game
moveTank SelfRole d g = do
  let c = nextCoord d (g ^. tank . tankCoord)
  let x = c ^. _x
  let y = c ^. _y
  if x >= 0 && x < width && y >= 0 && y < height && (c `notElem` g ^. walls)
     && (c `notElem` g ^. lakes) && (c /= _tankCoord (_enemy g) && (c `notElem` g ^. selfBase) && (c `notElem` g ^. enemyBase)) then
    g & tank . tankCoord .~ c & tank . tankDirection .~ d
  else
    g & tank . tankDirection .~ d
moveTank EnemyRole d g = do
  let c = nextCoord d (g ^. enemy . tankCoord)
  let x = c ^. _x
  let y = c ^. _y
  if x >= 0 && x < width && y >= 0 && y < height && (c `notElem` g ^. walls)
     && (c `notElem` g ^. lakes) && (c /= _tankCoord (_tank g) && (c `notElem` g ^. selfBase) && (c `notElem` g ^. enemyBase)) then
    g & enemy . tankCoord .~ c & enemy . tankDirection .~ d
  else
    g & enemy . tankDirection .~ d

buildWall :: Role -> Game -> Game
buildWall SelfRole g@Game{ _walls = ws} = do
  let c = nextCoord (g ^. tank . tankDirection) (g ^. tank . tankCoord)
  let x = c ^. _x
  let y = c ^. _y
  if x >= 0 && x < width && y >= 0 && y < height && (c `notElem` g ^. walls) && (c `notElem` g ^. stones)
    && (c `notElem` g ^. selfBase) && (c `notElem` g ^. enemyBase) && (c /= _tankCoord (_tank g)) && (c /= _tankCoord (_enemy g)) then
    g & walls .~ (initWall c : ws)
  else
    g & walls .~ ws
buildWall EnemyRole g@Game{ _walls = ws} = do
  let c = nextCoord (g ^. enemy . tankDirection) (g ^. enemy . tankCoord)
  let x = c ^. _x
  let y = c ^. _y
  if x >= 0 && x < width && y >= 0 && y < height && (c `notElem` g ^. walls) && (c `notElem` g ^. stones)
    && (c `notElem` g ^. selfBase) && (c `notElem` g ^. enemyBase) && (c /= _tankCoord (_tank g)) && (c /= _tankCoord (_enemy g)) then
    g & walls .~ (initWall c : ws)
  else
    g & walls .~ ws

setGameState :: Game -> GameState -> Game
setGameState g s = g & gameState .~ s


isGameOver :: Game -> Bool
isGameOver g = g ^. tank . tankHealth <= 0 || g ^. enemy . tankHealth <= 0 || g ^. tank . baseHealth <= 0 || g ^. enemy . baseHealth <= 0

isGameWon :: Game -> Bool
isGameWon g = g ^. enemy . tankHealth <= 0 || g ^. enemy . baseHealth <= 0

isGameLost :: Game -> Bool
isGameLost g = g ^. tank . tankHealth <= 0 || g ^. tank . baseHealth <= 0

-- | step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do 
    guard $ not (s ^. gameOver)
    collectAmmoTank <|> collectAmmoEnemy <|> die <|> hit <|> hitSelfBase <|> hitEnemyBase
     <|> attack <|> collect <|> blinkState <|> 
      MaybeT (Just <$> modify bulletsFly)


die :: MaybeT (State Game) ()
die = do
    MaybeT . fmap guard $ isGameOver <$> get
    MaybeT . fmap Just $ gameOver .= True

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
                                             b ^. bulletCoord  == w || b ^. bulletCoord == s 
                                             ]
    guard $ not $ null coordsToBeDel
    MaybeT . fmap Just $ do
        modifying bullets (delBullets coordsToBeDel)
        modifying walls (delWalls coordsToBeDel)

hitSelfBase :: MaybeT (State Game) ()
hitSelfBase = do
    bulletGetter <- use bullets
    selfBaseGetter <- use selfBase
    let coordsToBeDel = [ b ^. bulletCoord | b <- bulletGetter, 
                                             b ^. bulletCoord `elem` selfBaseGetter 
                                             ]
    guard $ not $ null coordsToBeDel
    MaybeT . fmap Just $ do
        modifying bullets (delBullets coordsToBeDel)
        modifying tank (reduceBaseHealth (head coordsToBeDel) selfBaseGetter)

hitEnemyBase :: MaybeT (State Game) ()
hitEnemyBase = do
    bulletGetter <- use bullets
    enemyBaseGetter <- use enemyBase
    let coordsToBeDel = [ b ^. bulletCoord | b <- bulletGetter, 
                                             b ^. bulletCoord `elem` enemyBaseGetter 
                                             ]
    guard $ not $ null coordsToBeDel
    MaybeT . fmap Just $ do
        modifying bullets (delBullets coordsToBeDel)
        modifying enemy (reduceBaseHealth (head coordsToBeDel) enemyBaseGetter)

blinkState :: MaybeT (State Game) ()
blinkState = do
    tankGetter <- use tank
    enemyGetter <- use enemy
    guard $ tankGetter ^. tankBlinkCount > 0 || enemyGetter ^. tankBlinkCount > 0
    MaybeT . fmap Just $ do
        modifying tank decreaseBlink
        modifying enemy decreaseBlink

decreaseBlink :: Tank -> Tank
decreaseBlink t= if t ^. tankBlinkCount > 0 then t & tankBlinkCount .~ (t ^. tankBlinkCount - 1) else t


delBullets :: [Coord] -> [Bullet] -> [Bullet]
delBullets coordsToBeDel bs = filter (\b -> not ((b ^. bulletCoord) `elem` coordsToBeDel)) bs

delWalls :: [Coord] -> [Coord] -> [Coord]
delWalls coordsToBeDel ws = filter (\w -> not ( w `elem` coordsToBeDel) ) ws


reduceBaseHealth :: Coord -> Base -> Tank -> Tank
reduceBaseHealth coordsToBeDel bs t = if coordsToBeDel `elem` bs then t & baseHealth %~ (\x -> x - (t ^. damageTaken)) else t

-- if tank is on collectible, collect it
collect :: MaybeT (State Game) ()
collect = do
    tankGetter <- use tank
    enemyGetter <- use enemy
    collectibleGetter <- use collectible
    guard $ (tankGetter ^. tankCoord == collectibleGetter ^. collectibleCoord) || (enemyGetter ^. tankCoord == collectibleGetter ^. collectibleCoord)
    MaybeT . fmap Just $ do
        modifying tank (collectCollectible collectibleGetter)
        modifying enemy (collectCollectible collectibleGetter)
        modifying collectible (addCollectible collectibleGetter)
        modifying collectible (deleteCollectible collectibleGetter)
        modifying collectible (lastCollectible collectibleGetter)


-- increase tank health by 20
collectCollectible :: Collectible -> Tank -> Tank
collectCollectible c t = t & tankHealth %~ (\h -> if h + (c ^. health) > 100 then 100 else h + (c ^. health))

-- the last collectible is has 30 additional health
lastCollectible :: Collectible -> Collectible -> Collectible
lastCollectible c c' = if c ^. coordinateIndex == 4  then c' & health .~ 50 else c'

-- increase collectible index by 1
addCollectible :: Collectible -> Collectible -> Collectible
addCollectible c c' = c' & coordinateIndex .~ (c ^. coordinateIndex + 1)

-- update collectible position according to coordinate list
deleteCollectible :: Collectible -> Collectible -> Collectible
deleteCollectible c c' = if c ^. coordinateIndex > 5 then c' & collectibleCoord .~ V2 (-1) (-1)
                         else c' & collectibleCoord .~ ((c ^. coordinateList) !! (c ^. coordinateIndex))

collectAmmoTank :: MaybeT (State Game) ()
collectAmmoTank = do
    tankGetter <- use tank
    ammoGetter <- use ammo
    guard $ tankGetter ^. tankCoord == ammoGetter ^. ammoCoord
    MaybeT . fmap Just $ do
        modifying enemy (collectAmmo ammoGetter)
        modifying ammo (addAmmo ammoGetter)
        modifying ammo (deleteAmmo ammoGetter)

collectAmmoEnemy :: MaybeT (State Game) ()
collectAmmoEnemy = do
    enemyGetter <- use enemy
    ammoGetter <- use ammo
    guard $ enemyGetter ^. tankCoord == ammoGetter ^. ammoCoord
    MaybeT . fmap Just $ do
        modifying tank (collectAmmo ammoGetter)
        modifying ammo (addAmmo ammoGetter)
        modifying ammo (deleteAmmo ammoGetter)

-- increase tank ammo by 5
collectAmmo :: Ammo -> Tank -> Tank
collectAmmo a t = t & damageTaken %~ (\h -> h + (a ^. ammoIncrease))

-- increase ammo index by 1
addAmmo :: Ammo -> Ammo -> Ammo
addAmmo a a' = a' & ammoIndex .~ (a ^. ammoIndex + 1)

-- update ammo position according to coordinate list
deleteAmmo :: Ammo -> Ammo -> Ammo
deleteAmmo a a' = if a ^. ammoIndex > 5 then a' & ammoCoord .~ V2 (-1) (-1)
                         else a' & ammoCoord .~ ((a ^. ammoList) !! (a ^. ammoIndex))



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
            modifying tank (increaseBlink coordsToBeDel)
            modifying enemy (hurt coordsToBeDel)
            modifying enemy (increaseBlink coordsToBeDel)
            modifying bullets (delBullets coordsToBeDel)

hurt :: [Coord] -> Tank -> Tank
hurt cs t= if t ^. tankCoord `elem` cs && t ^. tankHealth > 0
  then
    t & tankHealth .~ (t ^. tankHealth - t ^. damageTaken)
  else t

increaseBlink :: [Coord] -> Tank -> Tank
increaseBlink cs t= if t ^. tankCoord `elem` cs && t ^. tankHealth > 0
  then
    t & tankBlinkCount .~ (t ^. tankBlinkCount + 4)
  else t

-- | Get next position of bullets
bulletsFly :: Game -> Game
bulletsFly g = g & bullets %~ map moveBullet

moveBullet :: Bullet -> Bullet
moveBullet bullet@Bullet {_bulletDirection = bDir} = bullet & bulletCoord %~ moveCoord bDir False

fire :: Role -> Game -> Game 
fire SelfRole g@Game { _bullets = bs, _tank = t} = if not (g ^. gameOver) then g & bullets .~ newBullet else g
                                                        where
                                                            bulletCoord = moveCoord (t ^. tankDirection) False (t ^. tankCoord)
                                                            bulletDir = (t ^. tankDirection)
                                                            newBullet = (initBullet bulletCoord bulletDir : bs)
fire EnemyRole g@Game { _bullets = bs, _enemy = e} = if not (g ^. gameOver) then g & bullets .~ newBullet else g
                                                        where
                                                            bulletCoord = moveCoord (e ^. tankDirection) False (e ^. tankCoord)
                                                            bulletDir = e ^. tankDirection
                                                            newBullet = initBullet bulletCoord bulletDir : bs

-- TODO: (Maybe) add mine which explodes when tank is near

    
