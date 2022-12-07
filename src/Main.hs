module Main where

import Ammo
import Brick
import Brick.BChan (newBChan, writeBChan)
import Collectible
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((&), (.~))
import Control.Monad (forever, void)
import Game
import Global
import qualified Graphics.Vty as V
import Hitable
import Linear.V2 (V2 (..), _x, _y)
import Tank
import View

-- App definition

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g