{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = program

program :: Program
program = drawingOf pictureOfMaze

wall, ground, storage, box, boxInStorage :: Picture
-- box definition
box = colored brown (solidRectangle 1 1)
boxInStorage = colored (dark brown) box
-- ground definition
ground = colored (light yellow) (solidRectangle 1 1) 
-- wall definition
wall = colored gray (solidRectangle 1 1) 
-- storage definition
storage = colored red (solidCircle 0.15) & ground 

drawTile :: Int -> Picture
drawTile n
  | n == 1    = wall
  | n == 2    = ground
  | n == 3    = storage
  | n == 4    = box
  | otherwise = blank

maze :: Int -> Int -> Int
maze x y
  | abs x > 4  || abs y > 4  = 0  -- blank
  | abs x == 4 || abs y == 4 = 1  -- wall
  | x ==  2 && y <= 0        = 1  -- wall
  | x ==  3 && y <= 0        = 3  -- storage
  | x >= -2 && y == 0        = 4  -- box
  | otherwise                = 2  -- ground

mazeSize :: Int
mazeSize = 10

mazeDim :: [Int]
mazeDim = [-mazeSize..mazeSize]

drawDim :: Int -> Int -> Picture
drawDim x y = translated (fromIntegral x) (fromIntegral y) tile
            where tile = drawTile (maze x y)


pictureOfMaze :: Picture
pictureOfMaze = pictures [drawDim x y | x <- mazeDim, y <- mazeDim]


