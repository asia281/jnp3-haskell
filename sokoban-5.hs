{-# LANGUAGE OverloadedStrings #-}
import Data.String
import Data.Maybe
import System.IO

-- Types declaration
data Event = KeyPress String
type Screen = String

data Direction = R | U | L | D deriving (Enum, Eq)
data Coord = C Int Int deriving (Eq)
-- Maze is by default with moved boxes, so need to change it after start
data State = S {maze::Maze, dir::Direction, boxes::[Coord], ith::Int, moves::Int} 
instance Eq State where
  S _ d b i m == S _ d' b' i' m' = d == d' && b == b' && i == i' && m == m'
data Maze = Maze {curr::Coord, m::Coord->Tile}

newtype Program = IO ()
main :: IO ()

main = runActivity (resettable (withUndo (withStartScreen (Activity (loadMaze 1) handleRead draw))))

runActivity :: Activity world -> IO()
runActivity (Activity state handle draw) = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr (draw state) 
    go 0 state 
    where go n s = do
            c <- getChar
            let s' = if (newN n c) == 0 then handle (KeyPress [(newC n c)]) s
                     else s
            putStr "\ESCc"
            putStr (draw s') 
            go (newN n c) s'

-- Handle events
handleRead :: Event -> State -> State
handleRead (KeyPress [str]) s 
  | (str == 'N' || str == 'n' || isWinning s) && (ith s) < mazesLen = loadMaze ((ith s)+1)
  | str == 'W' || str == 'w' = adjacentCoord s {dir = U}
  | str == 'D' || str == 'd' = adjacentCoord s {dir = R}
  | str == 'A' || str == 'a' = adjacentCoord s {dir = L}
  | str == 'S' || str == 's' = adjacentCoord s {dir = D}
                     
handleRead _ s    = s
-- Load and init maze
initialBoxes :: Maze -> [Coord]
initialBoxes (Maze c m) = [(C x y) | x <- mazeX, y <- mazeY, 
                    m (C x y) == Box, reachable c (C x y) (adjacent (Maze c m))]

loadMaze :: Int -> State
loadMaze n = S (removeBoxes m) D (initialBoxes m) n 0
  where m = nth mazes n

-- Convert arrows to WASD.
convert :: Integer -> Char -> (Integer, Char)
convert n c
  | n == 0 && c == '\ESC' = (n+1, c)
  | n == 1 && c == '[' = (n+1, c)
  | n == 2 && c == 'A' = (0, 'W')
  | n == 2 && c == 'B' = (0, 'S')
  | n == 2 && c == 'C' = (0, 'D')
  | n == 2 && c == 'D' = (0, 'A')
  | otherwise = (0, c)

newN :: Integer -> Char -> Integer
newN n c = n'
    where (n', c') = convert n c 

newC :: Integer -> Char -> Char
newC n c = c'
    where (n', c') = convert n c 

draw :: State -> Screen
draw s = if isWinning s then winScreen s else pictureOfMaze (addBoxes (maze s) (boxes s))

-- Draw stuff
mazeX :: [Int]
mazeX = [-40..40]

mazeY :: [Int]
mazeY = [-11..11]

drawDim :: Maze -> Coord -> String
drawDim (Maze c maze) (C x y) 
 | x == 40 &&  drawTile (ifPlayer (Maze c maze) (C 0 y)) /= ""  =  "\n"
 | otherwise = drawTile (ifPlayer (Maze c maze) (C x y))

ifPlayer :: Maze -> Coord -> Tile
ifPlayer (Maze c maze) coord
  | coord == c && maze c == Storage = PlayerOnStorage
  | coord == c                      = Player
  | otherwise                       = maze coord

pictureOfMaze :: Maze -> Screen
pictureOfMaze maze = concat[ concat [drawDim maze (C x y) | x <- mazeX] | y <- [30,29..(-30)]]

data Activity world = Activity
    world
    (Event -> world -> world)
    (world -> Screen)
    
data SSState world = StartScreen | Running world deriving Eq

-- drawing funcs
type DrawFun = Int -> Int -> String
type Picture = DrawFun -> DrawFun
blank = id
(&) = (.)

data Tile = Wall | Ground | Storage | Box | BoxInStorage | Blank 
            | Player | PlayerOnStorage deriving (Eq)
            
drawTile :: Tile -> String
drawTile Wall            = "\ESC[30;45m#"
drawTile Ground          = "\ESC[92;40m "
drawTile Storage         = "\ESC[91;40m."
drawTile Box             = "\ESC[97;40m$"
drawTile BoxInStorage    = "\ESC[90;40m*"
drawTile Player          = "\ESC[92;40m@"
drawTile PlayerOnStorage = "\ESC[96;40m+"
drawTile Blank           = ""

translated :: Int -> Int -> Picture -> (DrawFun -> DrawFun)
translated x y pic =
  (\d x2 y2 -> pic d (x2 - x) (y2 + y))
  & (\d x2 y2 -> d (x2 + x) (y2 - y))

-- activities
resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw) = Activity state0 handle' draw
  where handle' (KeyPress [key]) _ | key == '\ESC' = state0
        handle' e s = handle e s

startScreen :: Screen
startScreen = "Sokoban!\n" ++ pictureOfBools ++ "\ESC[0m"

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
  where
    state0' = StartScreen
    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = WithUndo state0 []
    handle' (KeyPress [key]) (WithUndo s stack) | key == 'U' || key == 'u'
      = case stack of s':stack' -> WithUndo s' stack'
                      []          -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo s' (s:stack)
      where s' = handle e s

    draw' (WithUndo s _) = draw s

currentMaze :: [Coord] -> Maze -> Maze
currentMaze boxes maze = addBoxes (removeBoxes maze) boxes

-- Handling funcs
removeBoxes :: Maze -> Maze
removeBoxes (Maze c maze) = (Maze c maze')
  where maze' t | maze t == Box && reached          = Ground
                | maze t == BoxInStorage && reached = Storage
                | otherwise                         = maze t
                  where reached = reachable t c (adjacent (Maze c maze))

addBoxes :: Maze -> [Coord] -> Maze
addBoxes maze [] = maze
addBoxes (Maze i maze) (h:t) = addBoxes (Maze i maze') t 
  where maze' c | c == h && maze c == Storage = BoxInStorage
                | c == h                      = Box
                | otherwise                   = maze c
                
-- Move player
isAccess :: Coord -> Maze -> Bool
isAccess c (Maze _ m) = (m c == Storage || m c == Ground) 
 
isBoxAccess :: Coord -> Direction -> Maze -> Bool
isBoxAccess c d maze@(Maze _ m) = ((ifBox (m c) && isAccess (add c (move d)) maze))
  where ifBox t = (t == Box || t == BoxInStorage)
        
move :: Direction -> Coord
move R = (C 1 0)
move L = (C (-1) 0) 
move U = (C 0 1) 
move D = (C 0 (-1))

add :: Coord -> Coord -> Coord
add (C x y) (C z t) = (C (x+z) (y+t))

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go [initial] []
  where
    go [] _                               = True
    go (h:t) _ | not (isOk h)             = False
    go (h:t) visited | elemList h visited = go t visited
    go (h:t) visited = go (appendList t (adjacent h)) (h : visited) 

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = go [initial] []
  where
    go [] _                               = False
    go (h:t) _ | (v == h)                 = True
    go (h:t) visited | elemList h visited = go t visited
    go (h:t) visited = go (appendList t (neighbours h)) (h : visited) 
                               
allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList f vs
  where f a = reachable a initial neighbours

moveCoord :: Coord -> Direction -> Coord
moveCoord c d = add c (move d)

adjacent :: Maze -> Coord -> [Coord]
adjacent (Maze _ maze) c = filterList check (mapList (\d -> moveCoord c d) allDirs)
  where
    allDirs = [U, R, D, L]
    check x = (maze x /= Wall)

moveBox :: State -> [Coord] 
moveBox (S _ _ [] _ _) = []
moveBox (S maze d (b:bs) ith moves) = ifMoved
  where ifMoved | b == (curr maze) = ((moveCoord (curr maze) d) : bs)
                | otherwise = (b : moveBox(S maze d bs ith moves) )

adjacentCoord :: State -> State
adjacentCoord s@(S maze     d boxes ith moves)
  | isAccess moved fullMaze      = stateMoved
  | isBoxAccess moved d fullMaze = (S maze{curr = moved} d (moveBox stateMoved) ith (moves+1))
  | otherwise                    = s
  where fullMaze = addBoxes maze boxes
        moved = (add (curr maze) (move d))
        stateMoved = (S maze{curr = moved} d boxes ith (moves+1))

-- Win screen
winScreen :: State -> Screen
winScreen s | (ith s) < mazesLen && isWinning s 
            = "Poziom ukończony, liczba ruchów: " ++ show (moves s)  
            | isWinning s =  "Wygrałeś!"
            | otherwise  = ""

isWinning :: State -> Bool
isWinning (S (Maze _ maze) d boxes _ _) = allList f boxes
  where f x = maze x == Storage

-- Checking if maze is good
isClosed :: Maze -> Bool
isClosed (Maze c maze) = okTile && isGraphClosed c (adjacent (Maze c maze)) isOk
  where
    okTile = (maze c == Ground || maze c == Storage)
    isOk c = (maze c /= Blank) 

isSane :: Maze -> Bool
isSane (Maze c maze) = (go [c] [] Storage) >= (go [c] [] Box)
  where
    go [] _ _                                        = 0
    go (h:t) visited typ | elemList h visited        = go t visited typ
                         | (maze h == typ)           = 1 + goAdj
                         | otherwise = goAdj
      where goAdj = go (appendList t (adjacent (Maze h maze) h)) (h : visited) typ

isGood :: Maze -> Bool
isGood maze = isClosed maze && isSane maze

pictureOfBool :: Bool -> Char
pictureOfBool x
  | x = 'o'
  | otherwise = 'x'

pictureOfBools :: String
pictureOfBools = mapList pictureOfBool (mapList isGood mazes)

-- Lists funcs
foldList :: (a -> b -> b) -> b -> [a] -> b
foldList f b [] = b
foldList f b (x:xs) = f x (foldList f b xs)

elemList :: Eq a => a -> [a] -> Bool
elemList a l = foldList f False l
  where f x _ | x == a  = True
        f _ b = b

appendList :: [a] -> [a] -> [a]
appendList l1 l2 = foldList f l2 l1
  where f a l = (a:l) 

listLength :: [a] -> Int
listLength l = foldList f 0 l
  where f _ k = k + 1

filterList :: (a -> Bool) -> [a] -> [a]
filterList g l = foldList f [] l
  where f a l | g a = a : l
              | otherwise = l

nth :: [a] -> Int -> a
nth [] _ = error "list too short"
nth (h:t) i | i == 1    = h 
            | otherwise = nth t (i-1)

mapList :: (a -> b) -> [a] -> [b]
mapList g l = foldList f [] l
  where f x l = (g x) : l

andList :: [Bool] -> Bool
andList l = foldList (&&) True l 

allList :: (a -> Bool) -> [a] -> Bool
allList g l = foldList f True l
  where f x b | g x       = b
              | otherwise = False

-- Levels declaration
maze1 :: Coord -> Tile
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank   -- blank
  | abs x == 4 || abs y == 4 = Wall    -- wall
  | x ==  2 && y <= 0        = Wall    -- wall
  | x ==  3 && y <= 0        = Storage -- storage
  | x >= -2 && y == 0        = Box     -- box
  | otherwise                = Ground  -- ground

maze2 :: Coord -> Tile
maze2 (C x y)
  | abs x > 3  || abs y > 2                   = Blank
  | (x == 1 || x == 2) && y == 1              = Storage
  | (x == -1 && y == 1) || (x == 0 && y == 0) = Box
  | abs x == 3 || abs y == 2                  = Wall
  | otherwise                                 = Ground
  
maze3 :: Coord -> Tile
maze3 (C x y)
  | abs x > 3  || abs y > 5                 = Blank
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground

maze4 :: Coord -> Tile
maze4 (C x y)
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | y == 2                    = Wall
  | not (x == -2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 1 && y == 0          = Box
  | x == 1 && y == 3          = Box
  | otherwise                 = Ground

maze5 :: Coord -> Tile
maze5 (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

maze6 :: Coord -> Tile
maze6 (C x y)
  | abs x > 2  || abs y > 2  = Blank   -- blank
  | abs x == 2 || abs y == 2 = Wall    -- wall
  | x == 1 && y == 1         = Storage -- storage
  | x == 1 && y == 0         = Box     -- box
  | otherwise                = Ground  -- ground

mazes :: [Maze]
mazes = [(Maze (C 1 1) maze1), (Maze (C 1 0) maze2), (Maze (C 0 0) maze3), 
         (Maze (C 0 0) maze4), (Maze (C 0 3) maze5), (Maze (C 0 0) maze6)]

mazesLen :: Int
mazesLen = listLength mazes

badMaze1 :: Coord -> Tile
badMaze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank   -- blank
  | abs x == 4 || abs y == 4 = Wall    -- wall
  | x ==  2 && y <= 0        = Wall    -- wall
  | x ==  3 && y <= 0        = Storage -- storage
  | x >= -3 && y == 0        = Box     -- box
  | otherwise                = Ground  -- ground

badMaze2 :: Coord -> Tile
badMaze2 (C x y)
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | y == 2                    = Wall
  | not (x == -2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 1 && y == 3          = Box
  | x == 1 && y == 0          = Box
  | x == (-1) && y == 0       = Box
  | otherwise                 = Ground

badMaze3 :: Coord -> Tile
badMaze3 (C x y)
  | abs x > 3  || abs y > 2                   = Blank
  | (x == 1 || x == 2) && y == 1              = Storage
  | (x == -1 && y == 1) || (x == 0 && y == 0) = Box
  | otherwise                                 = Ground

badMazes :: [Maze]
badMazes = [(Maze (C 2 2) badMaze1), (Maze (C 0 0) badMaze2), 
            (Maze (C 1 0) badMaze3)]