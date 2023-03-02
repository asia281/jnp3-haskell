{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = walk3

program :: Program
program = drawingOf pictureOfMaze

data Direction = R | U | L | D deriving (Enum, Eq)
data Coord = C Int Int
data PlayerState = P Coord Direction
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


-- Part 1
initial1 :: Coord
initial1 = C 1 1

playerHalf :: Picture
playerHalf = translated 0 0.3 (circle 0.15)
          & polyline [(0, 0), (0.3, -0.1)] 
          & polyline [(0, -0.2), (0, 0.1)] 
          & polyline [(0, -0.2), (0.1, -0.5)]

player1 :: Picture
player1 = playerHalf & scaled (-1) 1 playerHalf

walk1 :: IO ()
walk1 = activityOf initial1 handleEvent1 drawState1

access :: Coord -> Coord -> Coord
access (C x y) (C a b)
  | maze c' == Storage || maze c' == Ground = c'
  | otherwise = (C x y)
  where c' = (C (x+a) (y+b))
 
add :: Coord -> Coord -> Coord
add (C x y) (C z t) = (C (x+z) (y+t))

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord d c
  | d == R = access c (C 1 0)
  | d == L = access c (C (-1) 0)
  | d == U = access c (C 0 1)
  | d == D = access c (C 0 (-1))

handleEvent1 :: Event -> Coord -> Coord
handleEvent1 (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up"    = adjacentCoord U c
    | key == "Left"  = adjacentCoord L c
    | key == "Down"  = adjacentCoord D c
handleEvent1 _ c = c

drawState1 :: Coord -> Picture
drawState1 (C x y) = pictures[atCoord (C x y) (player1), pictureOfMaze]

-- Part 2
walk2 :: IO ()
walk2 = activityOf initial2 handleEvent2 drawState2

initial2 :: PlayerState
initial2 = P (C 1 1) D

drawState2 :: PlayerState -> Picture
drawState2 (P (C x y) d) = pictures[atCoord (C x y) (player2 d), pictureOfMaze]


handleEvent2 :: Event -> PlayerState -> PlayerState
handleEvent2 (KeyPress key) (P c d)
    | key == "Right" = (P (adjacentCoord R c) R)
    | key == "Up"    = (P (adjacentCoord U c) U)
    | key == "Left"  = (P (adjacentCoord L c) L)
    | key == "Down"  = (P (adjacentCoord D c) D)
handleEvent2 _ p = p

playerR :: Picture
playerR = playerHalf
          & polyline [(0, 0),(0.3, 0.1)] 
          & polyline [(0, -0.2),(-0.1, -0.5)]

playerU :: Picture
playerU = translated 0 0.3 (solidCircle 0.15) & player1

player2 :: Direction -> Picture
player2 d
  | d == R = playerR
  | d == L = scaled (-1) 1 playerR
  | d == D = player1
  | d == U = playerU
  
-- Part 3
walk3 :: IO ()
walk3 = resettableActivityOf initial2 handleEvent2 drawState2

resettableActivityOf ::
    world ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
    
resettableActivityOf i handle d =  activityOf i handle' d 
        where handle' (KeyPress key) _ | key == "Esc" = i
              handle' e s = handle e s
              

-- Previous homework
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

data Tile = Wall | Ground | Storage | Box | Blank deriving (Enum, Eq)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank  -- blank
  | abs x == 4 || abs y == 4 = Wall  -- wall
  | x ==  2 && y <= 0        = Wall  -- wall
  | x ==  3 && y <= 0        = Storage  -- storage
  | x >= -2 && y == 0        = Box  -- box
  | otherwise                = Ground  -- ground

mazeSize :: Int
mazeSize = 10

mazeDim :: [Int]
mazeDim = [-mazeSize..mazeSize]

drawDim :: Coord -> Picture
drawDim (C x y) = translated (fromIntegral x) (fromIntegral y) tile
            where tile = drawTile (maze (C x y))


pictureOfMaze :: Picture
pictureOfMaze = pictures([drawDim (C x y) | x <- mazeDim, y <- mazeDim])


