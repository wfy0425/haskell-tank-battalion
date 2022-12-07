# haskell-tank-battalion
Final Project for UCSD CSE 230 Fall 2022

## Group Member
Fengyuan Wu A15535437  
Xinyuan Liang A59017604  
Na Sang A59001972  
Jiping Lin A15058075   

## Game
Tank Battalion is a classic shooter arcade video game in which the 2 players control tanks that defend their own base as well as destroy the other player's tank. The player will lose if either player's tank or base is destroyed by the other player. 

### Characters
The main characters of this game are tanks, where the player has one controllable tank that can move (up, down, left, and right) and fire, and enemies have several other tanks that are automatically controlled by computers. 

### Board
The board consists of the following elements: Ground, Walls, Bricks, Base and Tanks.

1. Grounds are general floors where tanks and bullets can move
2. Walls are impenetrable objects: tanks cannot move through a wall and walls cannot be destroyed by bullets fired by tanks
3. Bricks are destroyable objects: tanks cannot move through a brick; but when a brick is hit by a bullet, it becomes a ground
4. Tanks are either player/AI objects that represent the character, when it is hit by a bullet, it is destroyed; if the player’s tank is destroyed, the player loses
5. Base is the player’s objective that needs to be defended: when it is hit by bullets for a certain number of times, it is destroyed and the player loses the game

### Win/Lose Condition
The player will win if the player destroys all AI-controlling enemy tanks.
The player will lose under these two circumstances:
1. if player’s tank is destroyed by an AI-controlling enemy tank.
2. if player’s base is destroyed by an AI-controlling enemy tank.

## Architecture
This project aims to build a Haskell version of the Tank Battalion. The terminal user interface would be implemented using the brick library.
![UML](http://assets.processon.com/chart_image/638ce1cd7d9c082abaa044b7.png)

## Build & Run
```sh
stack build
stack run
```