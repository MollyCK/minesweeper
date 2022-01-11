module MinesweeperGame ( 
    generateMinefield
    , getCellAt

)
where

import Data.Char
import Data.List
import System.Random
import Data.Eq (Eq)
import Control.Monad (join)

-- Object setup --

type XYCors = (Int, Int) -- The minefield will be navigated by (X, Y) coordinates
type Board = [[Cell]] -- the minefield (game board) is implemented as a 2D space

data VisState = Visible            -- the visual state of a cell to the user
                | Unknown 
                | Flagged 
                | Potential
                deriving Eq
            
data Contents = Mine            -- the contents of a cell
                | Empty 
                deriving Eq

data Cell = Cell {
      state :: VisState
    , contents :: Contents
    , coords :: XYCors
} deriving Eq

-- Game setup --

bHeight = 10    -- board height (currently no user input)
bWidth = 10     -- board width (currently no user input)
quantMines = 20      -- total number of mines on board

-- initialise the board for the beginning of the game setup
boardInit :: Int -> Int -> Board       
boardInit = emptyBoard         -- create an empty board

-- create an (empty) board of cells with Empty contents and Unkown state
emptyBoard :: Int -> Int -> Board   
emptyBoard height width
        | height > 0 = emptyBoard (height - 2) width ++ [emptyRow (height-1) width]
        | otherwise = []

-- create an (empty) row of cells with Empty contents and Unkown state
emptyRow :: Int -> Int -> [Cell]    
emptyRow i n    | n > 0 = emptyRow i (n-2) ++ [Cell{state=Unknown, contents=Empty, coords=(i, n-1)}]
                | otherwise = []

-- construct a minefield with mines (not an empty board)
generateMinefield :: RandomGen g => g -> Board
generateMinefield g = loadMines (emptyBoard bHeight bWidth) (generateMines bHeight bWidth quantMines g)

-- load mines into their respective coordinates on the game board
loadMines :: Board -> [XYCors] -> Board
loadMines _ [] = []
loadMines board (xy : xys) = loadMines (setCellAt board xy cell) xys
    where cell = Cell{state=Unknown, contents=Mine, coords=xy}

-- NOT WORKING returns a list of coordinates for where mines are present on the board associated with random-generated number g
generateMines :: RandomGen g => Int -> Int -> Int -> g -> [XYCors]      
generateMines height width quantity g = []
{-                                      do
                                        x <- drawInt 0 height-1    -- keep an eye on this (getStdRandom) it's creating a new randomGenerator everytime so g is probably not used??
                                        y <- drawInt 0 width-1     
                                        remainder <- generateMines height width (quantity-1) g
                                        (x,y):remainder                     -- cons the remainder onto the end of the x and y coordinates for this mine
-}

-- Inspired from http://zvon.org/other/haskell/Outputrandom/getStdRandom_f.html
drawInt :: Int -> Int -> IO Int
drawInt lo hi = getStdRandom (randomR (lo, hi))

-- Cell Setters & Getters --

-- update cell at coordinates of currentCell to be newCell
setCell :: Board -> Cell -> Cell -> Board
setCell board currentCell newCell = setCellAt board (coords currentCell) newCell

-- update cell at coordinates of XYCors to be newCell
setCellAt :: Board -> XYCors -> Cell -> Board
setCellAt [] _ _ = []
setCellAt (row:rows) coords newCell = case coords of
        (0, j) -> (setCellRow row newCell j):rows
        (i, j) -> row:(setCellAt rows (i-1, j) newCell)

-- update cell in row (c:cs) at index yCoord to be newCell
setCellRow :: [Cell] -> Cell -> Int -> [Cell]
setCellRow [] _ _ = []
setCellRow (c:cs) newCell 0 = newCell:cs
setCellRow (c:cs) newCell yCoord = c:(setCellRow cs newCell (yCoord-1))

-- returns the Cell at point (x, y) or Nothing if the point is invalid
getCellAt :: Board -> XYCors -> Maybe Cell
getCellAt board (x, y)  | isValidPoint board (x, y) = Just $ board !! x !! y    -- if it's a valid point for the given board then get the Cell at board[i][j]
                        | otherwise = Nothing       -- else return nothing

-- returns a list of all the cells with mines on the boards
getAllMines :: Board -> [Cell]
getAllMines [] = []
getAllMines (row:rows) = (filter hasMine row) ++ (getAllMines rows)        -- take only the cells containg mines from the first row and concatenate with the recursion on the tail

-- returns a list of all the cells that are adjacent (horizontally, vertically, or diagonally) to cell0 
getAdjacents :: Board -> Cell -> [Cell]
getAdjacents (row:rows) cell0 = (filter (isAdjacent cell0) row) ++ (getAdjacents rows cell0)         -- take only the cells that are adjacent to cell0 from the first row and concatenate with the recursion on the tail

-- returns the number of mines that cell0 has adjacent to it
numAdjacentMines :: Board -> Cell -> Int
numAdjacentMines board cell0 = length $ filter (isAdjacent cell0)(getAllMines board)  -- we want the cardinal number of the intersection of the set of all mines and the set of adjacent cells to cell0

-- Utility functions --

-- test if cell1 is adjacent (horizontally, vertically, or diagonally) to cell0 
-- (Inspired by maths from: https://math.stackexchange.com/questions/478375/mathematical-formula-to-find-adjacent-items-in-a-grid)
isAdjacent :: Cell -> Cell -> Bool
isAdjacent cell0 cell1  | (y0 == y1) && (abs(x0 - x1) == 1) || (x0 == x1) && (abs(y0 - y1) == 1) = True        -- Two dots are adjacent (ignoring diagonals) if they agree on one coordinate, and differ by one in the other
                        | (abs(x0 - x1) == 1) && (abs(y0 - y1) == 1) = True    -- Two dots are adjacent diagonally if they differ by one in each coordinate
                        | otherwise = False
                        where   (x0, y0) = coords cell0
                                (x1, y1) = coords cell1

-- validate that the point is present on the game board (doesn't use hardcoded board measurements so can be used for a sub-section of a board given as a board)
isValidPoint :: Board -> XYCors -> Bool
isValidPoint board (x, y) = (x >= 0 && x < length board) && (y >= 0 && y < length board)

isVisible :: Cell -> Bool
isVisible Cell{state=Visible} = True
isVisible _ = False

isUnknown :: Cell -> Bool
isUnknown Cell{state=Unknown} = True
isUnknown _ = False

isFlagged :: Cell -> Bool
isFlagged Cell{state=Flagged} = True
isFlagged _ = False

isPotential :: Cell -> Bool
isPotential Cell{state=Potential} = True
isPotential _ = False

hasMine :: Cell -> Bool
hasMine Cell{contents=Mine} = True
hasMine _ = False