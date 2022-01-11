module MinesweeperGame ( 
    generateMinefield
    , 

)
where

import Data.Char
import Data.List
import System.Random
import Data.Eq (Eq)

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

setCellAt :: Board -> XYCors -> Cell -> Board
setCellAt [] _ _ = []
setCellAt (row:rows) coords cell = case coords of
        (0, j) -> (setCellRow row cell j):rows
        (i, j) -> row:(setCellAt rows (i-1, j) cell)

setCellRow :: [Cell] -> Cell -> Int -> [Cell]
setCellRow [] _ _ = []
setCellRow (c:cs) cell 0 = cell:cs
setCellRow (c:cs) cell yCoord = c:(setCellRow cs cell (yCoord-1))

-- Testing Cell states and contents --

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