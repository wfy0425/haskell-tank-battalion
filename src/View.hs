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

-- Types

-- | Ticks mark passing of time

-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Tank | Empty

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

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
  [ C.center $ drawGrid g ]

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Tank")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == g ^. tank      = Tank
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
-- drawCell Snake = withAttr snakeAttr cw
drawCell Tank  = withAttr tankAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (tankAttr, V.red `on` V.red)
  ]

tankAttr, emptyAttr :: AttrName
tankAttr = "tankAttr"
emptyAttr = "emptyAttr"
