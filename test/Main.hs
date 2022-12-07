module Main where

import System.Exit (exitFailure)
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-- import Main
import Game
import Global
import Bullet
import Collectible
import Ammo
import Data.Ratio
import Control.Lens.Operators
import Linear
import Tank
import Hitable

main :: IO ()
main = do
    g <- initGame
    defaultMain [
         testCase "test initGame" $ initGameTest g,
         testCase "test changeToIndexWorld" $ changeToIndexWorldTest g,
         testCase "test moveCoord normal" $ moveCoordTest1 g,
         testCase "test moveCoord boundary" $ moveCoordTest2 g,
         testCase "test moveTank north" $ moveTankTest1 g,
         testCase "test moveTank south" $ moveTankTest2 g,
         testCase "test buildWall self" $ buildWallTest1 g,
         testCase "test buildWall enemy" $ buildWallTest2 g,
         testCase "test fire self" $ fireTest1 g,
         testCase "test fire enemy" $ fireTest2 g,
         testCase "test step" $ stepTest g,
         testCase "test collect" collectTest1,
         testCase "test hit" hitTest1,
         testCase "test hit" hitTest2
        ]

initGameTest :: Game -> Assertion
initGameTest g = assertBool "test state for initGame" $ _gameState g == GameReady
    && _currentStageIdx g == 0

changeToIndexWorldTest :: Game -> Assertion
changeToIndexWorldTest g = assertBool "test state and idx for changeToIndexWorld"
    $ GameSelecting == _gameState (changeToIndexWorld 1 g)
    && 1 == _currentStageIdx (changeToIndexWorld 1 g)

moveCoordTest1 :: Game -> Assertion
moveCoordTest1 g = assertEqual "test moveCoord normal" (moveCoord North True (V2 0 0)) (V2 0 1)

moveCoordTest2 :: Game -> Assertion
moveCoordTest2 g = assertEqual "test moveCoord boundary" (moveCoord South True (V2 0 0)) (V2 0 0)

moveTankTest1 :: Game -> Assertion
moveTankTest1 g = do
    let g' = moveTank SelfRole North g
    assertEqual "test moveCoord" (_tank g')
        (Tank {_tankCoord = V2 17 3, _tankDirection = North, _tankHealth = 100, _baseHealth = 200, _tankBlinkCount = 0, _damageTaken = 10})

moveTankTest2 :: Game -> Assertion
moveTankTest2 g = do
    let g' = moveTank SelfRole South g
    assertEqual "test moveCoord" (_tank g')
        (Tank {_tankCoord = V2 17 1, _tankDirection = South, _tankHealth = 100, _baseHealth = 200, _tankBlinkCount = 0, _damageTaken = 10})

buildWallTest1 :: Game -> Assertion
buildWallTest1 g = do
    let g' = buildWall SelfRole g
    assertEqual "test buildWall self" (_walls g')
        [V2 17 3,V2 5 11,V2 5 12,V2 5 13,V2 5 14,V2 5 15,V2 5 16,V2 5 17,V2 5 18,V2 6 11,V2 6 18,V2 7 11,V2 7 18,V2 8 11,V2 8 18,V2 9 11,V2 9 18,V2 10 11,V2 10 18,V2 11 11,V2 11 18,V2 12 11,V2 12 18,V2 13 11,V2 13 18,V2 14 11,V2 14 18,V2 15 11,V2 15 12,V2 15 13,V2 15 14,V2 15 15,V2 15 16,V2 15 17,V2 15 18,V2 4 1,V2 4 2,V2 4 3,V2 4 4,V2 4 5,V2 4 6,V2 4 7,V2 4 8,V2 5 1,V2 5 8,V2 6 1,V2 6 8,V2 7 1,V2 7 8,V2 8 1,V2 8 8,V2 9 1,V2 9 8,V2 10 1,V2 10 8,V2 11 1,V2 11 8,V2 12 1,V2 12 8,V2 13 1,V2 13 8,V2 14 1,V2 14 2,V2 14 3,V2 14 4,V2 14 5,V2 14 6,V2 14 7,V2 14 8]

buildWallTest2 :: Game -> Assertion
buildWallTest2 g = do
    let g' = buildWall EnemyRole g
    assertEqual "test buildWall enemy" (_walls g')
        [V2 2 18,V2 5 11,V2 5 12,V2 5 13,V2 5 14,V2 5 15,V2 5 16,V2 5 17,V2 5 18,V2 6 11,V2 6 18,V2 7 11,V2 7 18,V2 8 11,V2 8 18,V2 9 11,V2 9 18,V2 10 11,V2 10 18,V2 11 11,V2 11 18,V2 12 11,V2 12 18,V2 13 11,V2 13 18,V2 14 11,V2 14 18,V2 15 11,V2 15 12,V2 15 13,V2 15 14,V2 15 15,V2 15 16,V2 15 17,V2 15 18,V2 4 1,V2 4 2,V2 4 3,V2 4 4,V2 4 5,V2 4 6,V2 4 7,V2 4 8,V2 5 1,V2 5 8,V2 6 1,V2 6 8,V2 7 1,V2 7 8,V2 8 1,V2 8 8,V2 9 1,V2 9 8,V2 10 1,V2 10 8,V2 11 1,V2 11 8,V2 12 1,V2 12 8,V2 13 1,V2 13 8,V2 14 1,V2 14 2,V2 14 3,V2 14 4,V2 14 5,V2 14 6,V2 14 7,V2 14 8]

fireTest1 :: Game -> Assertion
fireTest1 g = do
    let g' = fire SelfRole g
    assertEqual "test fire self" (_bullets g')
        [Bullet {_bulletCoord = V2 17 3, _bulletDirection = North, _tankDamage = 10, _enemyDamage = 10}]
fireTest2 :: Game -> Assertion
fireTest2 g = do
    let g' = fire EnemyRole g
    assertEqual "test fire enemy" (_bullets g')
        [Bullet {_bulletCoord = V2 2 18, _bulletDirection = North, _tankDamage = 10, _enemyDamage = 10}]

stepTest :: Game -> Assertion
stepTest g = assertEqual "test step" (_gameState(step g)) GameReady

collectTest1 ::  Assertion
collectTest1 = assertEqual "test collect right location" (collectCollectible Collectible{
    _collectibleCoord = V2 17 3
    , _coordinateList = [(V2 3 5), (V2 18 13), (V2 6 4), (V2 11 15), (V2 19 19), (V2 0 0)]
    , _coordinateIndex = 0
    , _health =  20
}
    Tank {_tankCoord = V2 17 3, _tankDirection = North, _tankHealth = 50, _baseHealth = 200, _tankBlinkCount = 0, _damageTaken = 10})
    Tank {_tankCoord = V2 17 3, _tankDirection = North, _tankHealth = 70, _baseHealth = 200, _tankBlinkCount = 0, _damageTaken = 10}


hitTest1 ::  Assertion
hitTest1 = assertEqual "test hit right location" (hurt [V2 17 3]
    Tank {_tankCoord = V2 17 3, _tankDirection = North, _tankHealth = 50, _baseHealth = 200, _tankBlinkCount = 0, _damageTaken = 10})
    Tank {_tankCoord = V2 17 3, _tankDirection = North, _tankHealth = 40, _baseHealth = 200, _tankBlinkCount = 0, _damageTaken = 10}
hitTest2 ::  Assertion
hitTest2 = assertEqual "test hit wrong location" (hurt [V2 1 1]
    Tank {_tankCoord = V2 17 3, _tankDirection = North, _tankHealth = 50, _baseHealth = 200, _tankBlinkCount = 0, _damageTaken = 10})
    Tank {_tankCoord = V2 17 3, _tankDirection = North, _tankHealth = 50, _baseHealth = 200, _tankBlinkCount = 0, _damageTaken = 10}
