{-# LANGUAGE OverloadedStrings #-}
module View where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Tank
import Hitable
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>), getContext
  , (<=>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Global
import Game
import Bullet
import Collectible
import Ammo
import Data.Ratio
import Control.Lens.Operators
-- Types


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                             = continue $ step g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'i') []))
  | _gameState g == GameRunning = continue $ moveTank SelfRole North g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') []))
  | _gameState g == GameRunning = continue $ moveTank SelfRole West g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') []))
  | _gameState g == GameRunning = continue $ moveTank SelfRole South g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') []))
  | _gameState g == GameRunning = continue $ moveTank SelfRole East g
  | otherwise = continue g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'o') []))
  | _gameState g == GameRunning = continue $ buildWall SelfRole g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') []))
  | _gameState g == GameRunning = continue $ moveTank EnemyRole North g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') []))
  | _gameState g == GameRunning = continue $ moveTank EnemyRole West g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') []))
  | _gameState g == GameRunning = continue $ moveTank EnemyRole South g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') []))
  | _gameState g == GameRunning = continue $ moveTank EnemyRole East g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'e') []))
  | _gameState g == GameRunning = continue $ buildWall EnemyRole g
  | otherwise = continue g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))
  | _gameState g == GameRunning = continue $ changeToIndexWorld sIdx g
  | _gameState g == GameSelecting = continue $ setGameState g GameReady
  | otherwise = halt g
  where
    sIdx = _currentStageIdx g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))
  | _gameState g == GameRunning = continue $changeToIndexWorld sIdx g
  | _gameState g == GameSelecting = continue $ setGameState g GameReady
  | otherwise = halt g
  where
    sIdx = _currentStageIdx g

handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') []))
  | _gameState g == GameRunning = continue $ fire EnemyRole g
  | otherwise = continue g
handleEvent g (VtyEvent (V.EvKey V.KEnter []))
  | _gameState g == GameReady = continue $ setGameState g GameSelecting
  | _gameState g == GameSelecting = continue $ setGameState g GameRunning
  | _gameState g == GameRunning = continue $ fire SelfRole g
  | otherwise = continue g

handleEvent g (VtyEvent (V.EvKey V.KLeft []))
  | _gameState g == GameSelecting =
        if sIdx > 0 then continue $ changeToIndexWorld (sIdx - 1) g else continue $ changeToIndexWorld (sl - 1) g
  | otherwise = continue g
  where
    sIdx = _currentStageIdx g
    sl = length (_stageData g)

handleEvent g (VtyEvent (V.EvKey V.KRight []))
  | _gameState g == GameSelecting =
        if sIdx < (sl - 1) then continue $ changeToIndexWorld (sIdx + 1) g else continue $ changeToIndexWorld 0 g
  | otherwise = continue g
  where
    sIdx = _currentStageIdx g
    sl = length (_stageData g)

handleEvent g _                                           = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g = case g^.gameState of
  GameReady -> drawWelcome g
  GameSelecting -> [ C.center $ padRight (Pad 2) (drawGameSelectingIns True) <+> drawGrid g <+> padLeft (Pad 2) (drawGameSelectingIns False)]
  _ ->  [ C.center $ padRight (Pad 2) (drawStats g False) <+> drawGrid g <+> padLeft (Pad 2) (drawStats g True)]

drawGameSelectingIns :: Bool -> Widget Name
drawGameSelectingIns True = hLimit 20
  $ vBox [
          padTop (Pad 2) $ str " " ,
          str " " ,
          str " " ,
          str " " ,
          padAll 1 $ vBox [  str "Enter: start" , str "←: previous" , str"→:next", str"Q: return"],
          str " " 
          ]
drawGameSelectingIns False = hLimit 20
  $ vBox [
          padTop (Pad 2) $ str " " ,
          padLeft (Pad 14) $ str " " 
          ]

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str $ "Stage: " ++ showN )
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCellFromGame g
    showN = if stageN < 10 then "0" ++ show stageN else show stageN
    stageN = _currentStageIdx g

drawCellFromGame :: Game -> Coord -> Widget Name
drawCellFromGame  g c
  | c `elem` g ^. stones      = drawStone
  | c `elem` bulletCoords     = drawBullet
  | c == tankCo               = drawTank SelfRole $ _tank g
  | c == enemyCo              = drawTank EnemyRole $ _enemy g
  | c == ammoCo               = drawAmmo $ _ammo g
  | c == collectCo            = drawCollectible $ _collectible g
  | c `elem` g ^. walls       = drawWall
  | c `elem` g ^. lakes       = drawLake
  | c `elem` g ^. selfBase    = drawSelfBase
  | c `elem` g ^. enemyBase   = drawEnemyBase
  
  | otherwise                 = drawEmpty
  where
      tankCo                  = _tankCoord $ _tank g
      enemyCo                 = _tankCoord $ _enemy g
      collectCo               = _collectibleCoord $ _collectible g
      ammoCo                  = _ammoCoord $ _ammo g
      bulletCoords            = [b ^. bulletCoord | b <- g ^. bullets]



drawTank :: Role -> Tank -> Widget Name
drawTank SelfRole tank = do
  if _tankBlinkCount tank >0 && (_tankBlinkCount tank % 2) == 1
  then drawEmpty
  else drawTank' SelfRole tank
drawTank EnemyRole tank =
  if _tankBlinkCount tank >0 && (_tankBlinkCount tank % 2) == 1
  then drawEmpty
  else drawTank' EnemyRole tank

drawTank' :: Role -> Tank -> Widget Name
drawTank' SelfRole tank =
  withAttr tankAttr $ case _tankDirection tank of
    North ->  tankNorthSquare
    South ->  tankSouthSquare
    East  ->  tankEastSquare
    West  ->  tankWestSquare
drawTank' EnemyRole tank =
  withAttr enemyAttr $ case _tankDirection tank of
    North ->  tankNorthSquare
    South ->  tankSouthSquare
    East  ->  tankEastSquare
    West  ->  tankWestSquare

drawWall :: Widget Name
drawWall  = withAttr wallAttr $ vBox [str "▤▤▤ ", str "▤▤▤ "]

drawBullet :: Widget Name
drawBullet  = withAttr bulletAttr  $ vBox [str " ●● ", str " ●● "]

drawStone :: Widget Name
drawStone  = withAttr stoneAttr $ vBox [str "▣▣▣ ", str "▣▣▣ "]

drawLake :: Widget Name
drawLake = withAttr lakeAttr $ vBox [str "~~~~",str "~~~~"]

drawEmpty :: Widget Name
drawEmpty = withAttr emptyAttr cw

drawSelfBase :: Widget Name
drawSelfBase = withAttr selfBaseAttr $ vBox [str " ⚑⚑ ",str " ⚑⚑ "]

drawEnemyBase :: Widget Name
drawEnemyBase = withAttr enemyBaseAttr $ vBox [str " ⚑⚑ ", str " ⚑⚑ "]

drawCollectible :: Collectible -> Widget Name
drawCollectible cc = if cc ^. health == 20
            then withAttr collectibleAttr amount20
            else withAttr collectibleAttr amount50

drawAmmo :: Ammo -> Widget Name
drawAmmo ammo = withAttr ammoAttr $ vBox [str " ⁍⁍ ", str " +5 "]

cw :: Widget Name
cw = vBox [str "    ", str "    "]

amount20 :: Widget Name
amount20 = vBox [str " ♥♥ ", str " 20 "]

amount50 :: Widget Name
amount50 = vBox [str " ♥♥ ", str " 50 "]


tankNorthSquare :: Widget Name
tankNorthSquare = vBox [str " ▲▲ ", str "    "]

tankSouthSquare :: Widget Name
tankSouthSquare = vBox [str "    ", str " ▼▼ "]

tankWestSquare :: Widget Name
tankWestSquare = vBox [str " ◀  ", str " ◀  "]

tankEastSquare :: Widget Name
tankEastSquare = vBox [str "  ▶ ", str "  ▶ "]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (tankAttr, V.white `on` V.red),
   (enemyAttr, V.white `on` V.blue),
   (wallAttr, wallColor),
   (stoneAttr, stoneColor),
   (lakeAttr, lakeColor),
    (bulletAttr, fg V.green `V.withStyle` V.bold),
  --  (gameOverAttr, V.white `V.withStyle` V.bold)
  (selfBaseAttr, V.brightYellow `on` V.red),
  (enemyBaseAttr, V.brightYellow `on` V.blue),
  (collectibleAttr, V.black `on` pinkColor),
  (ammoAttr, V.black `on` greenColor),
  (welcomeCharAttr, V.black `on` welcomeCharColor)
  ]

tankAttr, enemyAttr, wallAttr, stoneAttr, emptyAttr, selfBaseAttr, enemyBaseAttr :: AttrName
tankAttr = "tankAttr"
enemyAttr = "enemyAttr"
wallAttr = "wallAttr"
stoneAttr = "stoneAttr"
emptyAttr = "emptyAttr"
selfBaseAttr = "selfBaseAttr"
enemyBaseAttr = "enemyBaseAttr"

lakeAttr :: AttrName
lakeAttr = "lakeAttr"

collectibleAttr :: AttrName
collectibleAttr = "collectibleAttr"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

bulletAttr :: AttrName
bulletAttr = "bulletAttr"

ammoAttr :: AttrName
ammoAttr = "ammoAttr"

welcomeCharAttr :: AttrName
welcomeCharAttr = "welcomCharAttr"

drawStats :: Game -> Bool -> Widget Name
drawStats g True = hLimit 20
  $ vBox [
          padTop (Pad 2) $ drawTank SelfRole (_tank g),
          str $ "Health: " ++ show (g ^. tank ^. tankHealth),
          str $ "Base: " ++ show (g ^. tank ^. baseHealth),
          str $ "Damage: " ++ show (g ^. enemy ^. damageTaken),
          drawInstructions True,
          drawGameOver g
          ]
drawStats g False = hLimit 20
  $ vBox [
          padTop (Pad 2) $ drawTank EnemyRole (_enemy g),
          str $ "Health: " ++ show (g ^. enemy ^. tankHealth),
          str $ "Base: " ++ show (g ^. enemy ^. baseHealth),
          str $ "Damage: " ++ show (g ^. tank ^. damageTaken),
          drawInstructions False,
          drawGameOver g
  ]

-- drawStats :: Game -> Widget Name
-- drawStats g =
--   hLimit 22
--     $ withBorderStyle BS.unicodeBold
--     $ B.borderWithLabel (str "Stats")
--     $ vBox
--         [ drawStat "Score" $ g ^. score
--         , padTop (Pad 1) $ drawStat "Level" $ g ^. level
--         , drawLeaderBoard g
--         ]

-- drawStat :: String -> Int -> Widget Name
-- drawStat s n = padLeftRight 1 $ str s <+> padLeft Max (str $ show n)


drawInstructions :: Bool -> Widget Name
drawInstructions True = padAll 1
  $ vBox [  str "I: up" , str "K: down" , str"J: left", str"L: right",
            str "O: place brick",
            str "enter: shoot"
         ]
drawInstructions False = padAll 1
  $ vBox [  str "W: up" , str "S: down" , str"A: left", str"D: right",
            str "E: place brick",
            str "space: shoot"
         ]

-- drawGameOver :: Game -> Widget Name
-- drawGameOver g
--   | isGameWon g = padAll 1 $ vBox [
--       drawTank SelfRole (_tank g), str "Won!",
--             -- , str "r:restart"
--              str "q:quit"
--             ]
--   | isGameLost g = padAll 1 $ vBox [
--       drawTank EnemyRole (_enemy g), str "Won!",
--       -- , str "r:restart"
--        str "q:quit"
--       ]
--   | otherwise = emptyWidget

drawGameOver :: Game -> Widget Name
drawGameOver g
  | isGameWon g = padAll 1 $ vBox [
      drawTank SelfRole (_tank g), str "Won!",
            -- , str "r:restart"
             str "q:quit"
            ]
  | isGameLost g = padAll 1 $ vBox [
      drawTank EnemyRole (_enemy g), str "Won!",
      -- , str "r:restart"
       str "q:quit"
      ]
  | otherwise = emptyWidget







welcomeCharColor :: V.Color
welcomeCharColor = V.rgbColor 253 126 125

wallColor :: V.Attr
wallColor = V.rgbColor 255 153 0 `on` V.rgbColor 153 77 0

stoneColor :: V.Attr
stoneColor = V.rgbColor 166 166 166 `on` V.rgbColor 128 128 128

lakeColor :: V.Attr
lakeColor = V.blue `on` V.rgbColor 0 255 255

pinkColor :: V.Color
pinkColor = V.rgbColor 231 84 128

greenColor :: V.Color
greenColor = V.rgbColor 2 48 32

drawWelcome :: Game -> [Widget Name]
drawWelcome g = [ C.center $ vBox [C.hCenter welcomePaint, padTop (Pad 3) (welcomeText1 <=> welcomeText2)] ]
  where
    welcomeText1 = C.hCenter $ hLimit (34 * 2) $ str "Welcome to Tank!"
    welcomeText2 = C.hCenter $ hLimit (34 * 2) $ str "Press <enter> for new game | <q> to exit."
    welcomePaint = hBox (map dummyDraw "t a n k" )

dummyProcess :: [[Int]] -> Widget Name
dummyProcess grid = vBox $ map f grid
  where f arr = hBox $ map (\v -> if v == 1 then withAttr welcomeCharAttr oneS else oneS) arr

dummyDraw :: Char -> Widget Name
dummyDraw c = case c of
  't' -> dummyProcess [[1,1,1,1],[1,0,0,1],[0,1,1,0],[0,1,1,0],[0,1,1,0]]
  'a' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,1,1,1],[1,0,0,1],[1,0,0,1]]
  'n' -> dummyProcess [[1,1,1,1],[1,0,0,1],[1,0,0,1],[1,0,0,1],[1,0,0,1]]
  'k' -> dummyProcess [[1,0,0,1],[1,0,1,0],[1,1,0,0],[1,0,1,0],[1,0,0,1]]
  ' ' -> dummyProcess $ replicate 5 [0]
  _ -> dummyProcess $ replicate 5 [1,1,1,1]

oneS :: Widget Name
oneS = str "  "