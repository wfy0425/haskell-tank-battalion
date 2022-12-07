module Main where

import System.Exit (exitFailure)
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

-- import Main
import Game
import Global

main :: IO ()
main = do
    g <- initGame
    defaultMain [
         testCase "test init world state" $ initWTest g
        ]

initWTest :: Game -> Assertion
initWTest g = assertBool "wrong state of init world" $ _gameState g == GameReady