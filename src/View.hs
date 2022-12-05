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
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Global
import Game
import Bullet
import Collectible
import Data.Ratio
import Control.Lens.Operators
-- Types


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                             = continue $ step g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'i') []))               = continue $ moveTank SelfRole North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') []))             = continue $ moveTank SelfRole West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') []))             = continue $ moveTank SelfRole South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') []))            = continue $ moveTank SelfRole East g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'o') []))       = continue $ buildWall SelfRole g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') []))       = continue $ moveTank EnemyRole North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') []))       = continue $ moveTank EnemyRole West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') []))       = continue $ moveTank EnemyRole South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') []))       = continue $ moveTank EnemyRole East g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'e') []))       = continue $ buildWall EnemyRole g

-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))       = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))              = halt g
handleEvent g (VtyEvent (V.EvKey V.KEnter []))            = continue $ fire SelfRole g
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') []))       = continue $ fire EnemyRole g
handleEvent g _                                           = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g False) <+> drawGrid g <+> padLeft (Pad 2) (drawStats g True),
    drawWall
  ]
-- drawUI g =
--   [ C.vCenter $ vLimit 22 $ hBox
--       [ padLeft Max $ padRight (Pad 2) $ drawStats (ui ^. game)
--       , drawGrid g
--       , padRight Max $ padLeft (Pad 2) $ drawInfo (ui ^. game)
--       ]
--   ]

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Tank")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCellFromGame g

drawCellFromGame :: Game -> Coord -> Widget Name
drawCellFromGame  g c
  | c == collectCo            = drawCollectible $ _collectible g 
  | c == tankCo               = drawTank SelfRole $ _tank g
  | c == enemyCo              = drawTank EnemyRole $ _enemy g
  | c `elem` g ^. walls       = drawWall
  | c `elem` g ^. stones      = drawStone
  | c `elem` bulletCoords     = drawBullet
  | c `elem` g ^. selfBase    = drawSelfBase
  | c `elem` g ^. enemyBase   = drawEnemyBase
  | otherwise                 = drawEmpty
  where
      tankCo                  = _tankCoord $ _tank g
      enemyCo                 = _tankCoord $ _enemy g
      collectCo               = _collectibleCoord $ _collectible g
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
drawWall  = withAttr wallAttr cw

drawBullet :: Widget Name
drawBullet  = withAttr bulletAttr cw

drawStone :: Widget Name
drawStone  = withAttr stoneAttr cw

drawEmpty :: Widget Name
drawEmpty = withAttr emptyAttr cw

drawSelfBase :: Widget Name
drawSelfBase = withAttr selfBaseAttr cw

drawEnemyBase :: Widget Name
drawEnemyBase = withAttr enemyBaseAttr cw

drawCollectible :: Collectible -> Widget Name
drawCollectible cc = withAttr collectibleAttr amount

cw :: Widget Name
cw = str "  "


star :: Widget Name
star = str "O"

amount :: Widget Name
amount = str "20"


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (tankAttr, V.black `on` V.red),
   (enemyAttr, V.black `on` V.blue),
   (wallAttr, V.black `on` V.white),
   (stoneAttr, V.black `on` V.brightYellow),
   (bulletAttr, V.black `on` V.green),
  --  (gameOverAttr, V.white `V.withStyle` V.bold)
  (selfBaseAttr, V.black `on` V.red),
  (enemyBaseAttr, V.black `on` V.blue),
  (collectibleAttr, V.black `on` V.yellow)
  ]

tankAttr, enemyAttr, wallAttr, stoneAttr, emptyAttr, selfBaseAttr, enemyBaseAttr :: AttrName
tankAttr = "tankAttr"
enemyAttr = "enemyAttr"
wallAttr = "wallAttr"
stoneAttr = "stoneAttr"
emptyAttr = "emptyAttr"
selfBaseAttr = "selfBaseAttr"
enemyBaseAttr = "enemyBaseAttr"

collectibleAttr :: AttrName
collectibleAttr = "collectibleAttr"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

bulletAttr :: AttrName
bulletAttr = "bulletAttr"

drawStats :: Game -> Bool -> Widget Name
drawStats g True = hLimit 20
  $ vBox [
          padTop (Pad 2) $ drawTank SelfRole (_tank g),
          str $ "Lives: " ++ show (g ^. tank ^. tankHealth),
          str $ "Base: " ++ show (g ^. tank ^. baseHealth),
          drawInstructions True,
          drawGameOver g
          ]
drawStats g False = hLimit 20
  $ vBox [
          padTop (Pad 2) $ drawTank EnemyRole (_enemy g),
          str $ "Lives: " ++ show (g ^. enemy ^. tankHealth),
          str $ "Base: " ++ show (g ^. enemy ^. baseHealth),
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
  $ vBox [  str "i: up" , str "k: down" , str"j: left", str"l: right"
            ,str "enter: shoot"
         ]
drawInstructions False = padAll 1
  $ vBox [  str "W: up" , str "S: down" , str"A: left", str"D: right"
            ,str "space: shoot"
         ]

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



tankNorthSquare :: Widget Name
tankNorthSquare = vBox [str " ↑"]

tankSouthSquare :: Widget Name
tankSouthSquare = vBox [str " ↓"]

tankWestSquare :: Widget Name
tankWestSquare = vBox [str " ←"]

tankEastSquare :: Widget Name
tankEastSquare = vBox [str " →"]