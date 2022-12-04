{-# LANGUAGE OverloadedStrings #-}
module View where

-- import Control.Monad (forever, void)
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Concurrent (threadDelay, forkIO)
-- import Data.Maybe (fromMaybe)

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
-- import Control.Lens ((^.))
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
          -- , appChooseCursor = neverShowCursor
          -- , appHandleEvent = handleEvent
          -- , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  g <- initGame


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
-- snakeAttr = "snakeAttr"
tankAttr = "tankAttr"
emptyAttr = "emptyAttr"
