module Main
  ( main
  ) where

import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)

import qualified Graphics.Vty as V
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((.~), (&))
import Brick
import Brick.BChan (newBChan, writeBChan)

import Tank
import View
import Global
import Game
import Hitable
import Collectible



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
              _tank  = initTank (width - 3) 2 
              , _enemy = initTank 2 (height-3)
              , _walls = initWalls
              , _stones = initStones
              , _bullets = []
              , _selfBase = initBase SelfRole
              , _enemyBase = initBase EnemyRole
              , _gameOver = False
              , _collectible = initCollectible (width - 3) 5
              , _gameState = GameReady
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
-- - [X] make move functionality work
-- - [X] direction
-- - [X] Add health

-- - [X] Add bullets

-- gameboard
-- - [X] Add walls
-- - [X] Add score
-- - [ ] Add game over
-- - [ ] Add game win
-- - [X] Add game start
-- - [ ] Add game pause
-- - [ ] Add game restart
-- - [X] Add game quit
-- - [ ] Add game menu
-- - [ ] UI
-- - [ ] Add maps
-- - [X] build walls

-- - [ ] items
-- - - [ ] armor
-- - - [ ] gain health
-- - - [ ] big bullet
-- - - [ ] powerful bullet


-- - [ ] add water
-- - - [ ] add enchanced item to cross water 

-- [ ] UI
-- - [ ] Welcome screen
-- - [ ] Game over screen
-- - [ ] Game win screen
-- - [ ] Game pause screen
-- - [ ] map selection screen
-- - [ ] more instruction
-- - [X] hit animation





-- [ ] Unit tests
-- [ ] Presentation 