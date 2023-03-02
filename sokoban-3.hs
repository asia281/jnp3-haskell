{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = entireGame

data Direction = R | U | L | D deriving (Enum, Eq)
data Coord = C Int Int deriving (Eq)
data State = S {coord::Coord, dir::Direction, boxes::[Coord]}
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

-- Part 1, 2: Initial state
initialBoxes :: [Coord]
initialBoxes = [(C x y) | x <- mazeDim, y <- mazeDim, mazeInit (C x y) == Box]

initialState :: State
initialState = S (C 1 1) D initialBoxes

-- Part 3: Move boxes
ifBox :: Tile -> Bool
ifBox t = t == Box || t == BoxInStorage

removeBoxes :: Maze -> Coord -> Tile
removeBoxes maze = f . maze 
  where f t | ifBox t   = Ground
            | otherwise = t
 
type Maze = Coord -> Tile

addBoxes :: Maze -> [Coord] -> Maze
addBoxes maze [] = maze
addBoxes maze (h:t) = addBoxes (maze') t 
  where maze' c | c == h && maze c == Storage = BoxInStorage
                | c == h                      = Box
                | otherwise                   = maze c

-- Part 4: draw the state
currentMaze :: [Coord] -> Maze
currentMaze boxes = addBoxes (removeBoxes mazeInit) boxes

draw :: State -> Picture
draw (S c d boxes) = winScreen (S c d boxes) 
                   & atCoord c (player d) 
                   &  pictureOfMaze (currentMaze boxes)

-- Part 5: handle events
handleEvent :: Event -> State -> State

handleEvent (KeyPress key) s
    | key == "Right" = adjacentCoord s {dir = R}
    | key == "Up"    = adjacentCoord s {dir = U}
    | key == "Left"  = adjacentCoord s {dir = L}
    | key == "Down"  = adjacentCoord s {dir = D}
handleEvent _ s      = s

isAccess :: Coord -> Maze -> Bool
isAccess c m = (m c == Storage || m c == Ground) 
 
isBoxAccess :: Coord -> Direction -> Maze -> Bool
isBoxAccess c d m = ((ifBox (m c) && isAccess (add c (moves d)) m))

moves :: Direction -> Coord
moves R = (C 1 0)
moves L = (C (-1) 0) 
moves U = (C 0 1) 
moves D = (C 0 (-1))

add :: Coord -> Coord -> Coord
add (C x y) (C z t) = (C (x+z) (y+t))

moveBox :: State -> [Coord] 
moveBox (S _ _ []) = []
moveBox (S moved d (b:bs)) = ifMoved
  where ifMoved | b == moved = ((add moved (moves d)) : bs)
                | otherwise = (b : moveBox(S moved d bs))

adjacentCoord :: State -> State
adjacentCoord (S c d boxes)
  | isAccess moved (currentMaze boxes)      = stateMoved
  | isBoxAccess moved d (currentMaze boxes) = (S moved d (moveBox stateMoved))
  | otherwise                               = (S c d boxes)
  where moved = (add c (moves d))
        stateMoved = (S moved d boxes)

-- Part 6
startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!") 
             & translated 0 (-4) (lettering "Press space to start the game")

data SSState world = StartScreen | Running world

data Activity world = Activity {
        actState  :: world,
        actHandle :: (Event -> world -> world), 
        actDraw   ::(world -> Picture)}
        
resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw) = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
    
runActivity :: Activity s -> IO ()
runActivity (Activity s h d) = activityOf s h d

game :: Activity State
game = Activity initialState handleEvent draw

entireGame :: IO()
entireGame = runActivity (resettable (withStartScreen game))

-- Part 7
winScreen :: State -> Picture
winScreen s | isWinning s = scaled 2 2 (lettering "Wygrałeś!")
      | otherwise  = blank

isWinning :: State -> Bool
isWinning (S c d boxes) = allList boxes

allList :: [Coord] -> Bool
allList [] = True
allList (h:t) = ((mazeInit h) == Storage) && allList t

-- Player Definiton
playerHalf :: Picture
playerHalf = translated 0 0.3 (circle 0.15)
          & polyline [(0, 0), (0.3, -0.1)] 
          & polyline [(0, -0.2), (0, 0.1)] 
          & polyline [(0, -0.2), (0.1, -0.5)]

playerD :: Picture
playerD = playerHalf & scaled (-1) 1 playerHalf

playerR :: Picture
playerR = playerHalf
          & polyline [(0, 0),(0.3, 0.1)] 
          & polyline [(0, -0.2),(-0.1, -0.5)]

playerU :: Picture
playerU = translated 0 0.3 (solidCircle 0.15) & playerD

player :: Direction -> Picture
player d
  | d == R = playerR
  | d == L = scaled (-1) 1 playerR
  | d == D = playerD
  | d == U = playerU

-- Maze definition
wall, ground, storage, box, boxInStorage :: Picture
box = colored brown (solidRectangle 1 1)
boxInStorage = colored (dark brown) box
ground = colored (light yellow) (solidRectangle 1 1) 
wall = colored gray (solidRectangle 1 1) 
storage = colored red (solidCircle 0.15) & ground 

data Tile = Wall | Ground | Storage | Box | BoxInStorage | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall         = wall
drawTile Ground       = ground
drawTile Storage      = storage
drawTile Box          = box
drawTile BoxInStorage = boxInStorage
drawTile Blank        = blank

mazeInit :: Coord -> Tile
mazeInit (C x y)
  | abs x > 4  || abs y > 4  = Blank   -- blank
  | abs x == 4 || abs y == 4 = Wall    -- wall
  | x ==  2 && y <= 0        = Wall    -- wall
  | x ==  3 && y <= 0        = Storage -- storage
  | x >= -2 && y == 0        = Box     -- box
  | otherwise                = Ground  -- ground

mazeSize :: Int
mazeSize = 10

mazeDim :: [Int]
mazeDim = [-mazeSize..mazeSize]

drawDim :: Maze -> Coord -> Picture
drawDim maze (C x y) = translated (fromIntegral x) (fromIntegral y) tile
            where tile = drawTile (maze (C x y))

pictureOfMaze :: Maze -> Picture
pictureOfMaze maze = pictures([drawDim maze (C x y) | x <- mazeDim, y <- mazeDim])


