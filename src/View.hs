{-# LANGUAGE OverloadedStrings #-}
module View where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Tank

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
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
-- Types

data Cell = TankCell | EnemyCell | WallCell | StoneCell | BulletCell | EmptyCell 

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ moveTank North g 
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ moveTank South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ moveTank East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveTank West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') []))         = continue $ moveEnemy North g 
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') []))       = continue $ moveEnemy South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') []))      = continue $ moveEnemy East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') []))       = continue $ moveEnemy West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey V.KEnter []))         = continue $ fire SelfRole g 
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') []))      = continue $ fire EnemyRole g 
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g False) <+> drawGrid g <+> padLeft (Pad 2) (drawStats g True),
    drawCell WallCell
  ]

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Tank")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    bulletCoords = [b ^. bulletCoord | b <- g ^. bullets]
    cellAt c
      | c == g ^. tank ^. tankCoord  = TankCell
      | c == g ^. enemy ^. tankCoord  = EnemyCell
      | c `elem` g ^. walls = WallCell
      | c `elem` g ^. stones = StoneCell
      | c `elem` bulletCoords = BulletCell
      | otherwise           = EmptyCell

drawCell :: Cell -> Widget Name
-- drawCell Snake = withAttr snakeAttr cw
drawCell TankCell = withAttr tankAttr cw
drawCell EnemyCell  = withAttr enemyAttr cw
drawCell EmptyCell = withAttr emptyAttr cw
drawCell WallCell = withAttr wallAttr cw
drawCell BulletCell = withAttr bulletAttr cw
drawCell StoneCell = withAttr stoneAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (tankAttr, V.red `on` V.red), 
   (enemyAttr, V.blue `on` V.blue),
   (wallAttr, V.white `on` V.white),
   (stoneAttr, V.brightYellow `on` V.brightYellow),
   (bulletAttr, V.green `on` V.green)
  --  (gameOverAttr, V.white `V.withStyle` V.bold)
  ]

tankAttr, emptyAttr :: AttrName
tankAttr = "tankAttr"
enemyAttr = "enemyAttr"
wallAttr = "wallAttr"
stoneAttr = "stoneAttr"
emptyAttr = "emptyAttr"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

bulletAttr :: AttrName
bulletAttr = "bulletAttr"

drawStats :: Game -> Bool -> Widget Name
drawStats g True = hLimit 20
  $ vBox [ padTop (Pad 2) $ drawCell TankCell
          ,str $ "Lives: " ++ show (g ^. tank ^. tankHealth)
          , drawInstructions True
          , drawGameOver g
          ]
drawStats g False = hLimit 20
  $ vBox [padTop (Pad 2) $ drawCell EnemyCell
          ,str $ "Lives: " ++ show (g ^. enemy ^. tankHealth)
          , drawInstructions False
           , drawGameOver g
  ]

drawInstructions :: Bool -> Widget Name
drawInstructions True = padAll 1
  $ vBox [  str "↑: up" , str "↓: down" , str"←: left", str"→: right"
            ,str "space: shoot"
         ]
drawInstructions False = padAll 1
  $ vBox [  str "W: up" , str "S: down" , str"A: left", str"D: right"
            ,str "enter: shoot"
         ]

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if (isGameWon g)
    then padAll 1
      $ vBox [  drawCell TankCell, str "Won!"
              -- , str "r:restart"
              , str "q:quit"
              ]
    else if (isGameLost g)
        then padAll 1
          $ vBox [  drawCell EnemyCell, str "Won!"
            -- , str "r:restart"
            , str "q:quit"
            ]
        else emptyWidget
          