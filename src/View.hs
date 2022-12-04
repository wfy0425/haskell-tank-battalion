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

-- Types

data Cell = Tank | Enemy | Wall | Stone | Empty 


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
-- handleEvent g (AppEvent Tick)                       = continue $ step g
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
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g False) <+> drawGrid g <+> padLeft (Pad 2) (drawStats g True),
    drawCell Wall
  ]

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Tank")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == g ^. tank ^. tankCoord  = Tank
      | c == g ^. enemy ^. tankCoord  = Enemy
      | c `elem` g ^. walls = Wall
      | c `elem` g ^. stones = Stone
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
-- drawCell Snake = withAttr snakeAttr cw
drawCell Tank  = withAttr tankAttr cw
drawCell Enemy  = withAttr enemyAttr cw
drawCell Empty = withAttr emptyAttr cw
drawCell Wall = withAttr wallAttr cw
drawCell Stone = withAttr stoneAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (tankAttr, V.red `on` V.red), 
   (enemyAttr, V.blue `on` V.blue),
   (wallAttr, V.white `on` V.white),
   (stoneAttr, V.brightYellow `on` V.brightYellow)
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

drawStats :: Game -> Bool -> Widget Name
drawStats g True = hLimit 20
  $ vBox [ padTop (Pad 2) $ drawCell Tank
          ,str $ "Lives: " ++ show (g ^. tank ^. tankHealth)
          , drawInstructions True
          , drawGameOver g
          ]
drawStats g False = hLimit 20
  $ vBox [padTop (Pad 2) $ drawCell Enemy
          ,str $ "Lives: " ++ show (g ^. enemy ^. tankHealth)
          , drawInstructions False
           , drawGameOver g
  ]

drawInstructions :: Bool -> Widget Name
drawInstructions True = padAll 1
  $ vBox [  str "↑: up" , str "↓: down" , str"←: left", str"→: right"
            ,str "K: shoot"
         ]
drawInstructions False = padAll 1
  $ vBox [  str "W: up" , str "S: down" , str"A: left", str"D: right"
            ,str "space: shoot"
         ]

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if (isGameWon g)
    then padAll 1
      $ vBox [  drawCell Tank, str "Won!"
              -- , str "r:restart"
              , str "q:quit"
              ]
    else if (isGameLost g)
        then padAll 1
          $ vBox [  drawCell Enemy, str "Won!"
            -- , str "r:restart"
            , str "q:quit"
            ]
        else emptyWidget
          