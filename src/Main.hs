module Main
  ( main
  ) where

import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)

import qualified Graphics.Vty as V
import Control.Lens ((.~), (&))
import Brick
import Tank
import View
import Global
import Brick.BChan (newBChan, writeBChan)

-- type Name = ()

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }


-- | Initialize a paused game with random food location
initGame :: Game
initGame = Game {
              _tank  = initTank
              , _enemy = initTank
              , _walls = []
              , _bullets = []
            }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  -- g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app initGame


-- TODO:

-- Tank
-- - make move functionality work
-- - direction
-- - Add health

-- - Add bullets

-- gameboard
-- - Add walls
-- - Add score
-- - Add game over
-- - Add game win
-- - Add game start
-- - Add game pause
-- - Add game restart
-- - Add game quit
-- - Add game menu
-- - UI