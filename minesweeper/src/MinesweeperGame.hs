{-# LANGUAGE FlexibleInstances #-}

module MinesweeperGame( 
    Board
    , Cell (state, contents, coords)
    , VisState (Visible, Unknown, Flagged, Questioning)
    , generateBoard
    , unsafeGetCellAt
    , numAdjMines
    , isAdjacent
    , isFlagged
    , isVisible
    , isGameComplete
    , isEndGame
    , revealCell
    , revealBlankArea
    , flagCell
    , questionCell
    , toStringCell
) where


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
                | Questioning
                deriving (Eq, Show)
    
data Contents = Mine            -- the contents of a cell
                | Empty 
                deriving (Eq, Show)

data Cell = Cell {
      state :: VisState
    , contents :: Contents
    , coords :: XYCors
} deriving (Eq, Show)

{- Objects toString designed with Command Line in mind-}

toStringBoard :: Board -> String
toStringBoard [] = ""
toStringBoard (row:rows) = (toStringRow (row:rows) row) ++ "\n" ++ toStringBoard rows

toStringRow :: Board -> [Cell] -> String
toStringRow _ [] = ""
toStringRow [] _ = ""
toStringRow board (cell:cells) = case state cell of 
                                Unknown -> '#':toStringRow board cells
                                Flagged -> '!':toStringRow board cells
                                Questioning -> '?':toStringRow board cells
                                Visible -> (toStringCell board cell) ++ (toStringRow board cells)

toStringCell :: Board -> Cell -> String
toStringCell board cell0 = case contents cell0 of
                            Mine -> "*"
                            Empty -> case numAdjMines board cell0 of
                                        0 -> " "
                                        n -> " " ++ [intToDigit n] ++ " "

{- Game setup -}

bHeight = 20    -- board height (currently no user input)
bWidth = 20     -- board width (currently no user input)
quantMines = 60      -- total number of mines on board

-- initialise the board for the beginning of the game setup
boardInit :: Int -> Int -> Board       
boardInit = emptyBoard         -- create an empty board

-- create an (empty) board of cells with Empty contents and Unkown state
emptyBoard :: Int -> Int -> Board   
emptyBoard height width
        | height > 0 = emptyBoard (height - 1) width ++ [emptyRow (height-1) width]
        | otherwise = []

-- create an (empty) row of cells with Empty contents and Unkown state
emptyRow :: Int -> Int -> [Cell]    
emptyRow i n    | n > 0 = emptyRow i (n-1) ++ [Cell{state=Unknown, contents=Empty, coords=(i, n-1)}]
                | otherwise = []

-- construct a board with mines (not an empty board)
generateBoard :: Int -> Int -> IO Board
generateBoard height width = do
    let board = emptyBoard height width
    mineCoords <- generateMines height width quantMines
    return $ loadMines board mineCoords

-- load mines into their respective coordinates on the game board
loadMines :: Board -> [XYCors] -> Board
loadMines board [] = board
loadMines board (xy : xys) = loadMines (setCellAt board xy cell) xys
    where cell = Cell{state=Unknown, contents=Mine, coords=xy}

-- Random Numbers and Mine Generation --

generateMines :: Int -> Int -> Int -> IO([XYCors])
generateMines height width 0 = return []
generateMines height width n = do
                                x <- randomRIO (0, height-1)
                                y <- randomRIO (0, width-1)
                                remainder <- generateMines height width (n-1)
                                return $ (x, y):remainder

{- Cell Setters & Getters -}

    -- Points & Coordinates --

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

unsafeGetCellAt :: Board -> XYCors -> Cell
unsafeGetCellAt board (x, y) = board !! x !! y

    -- States --
        -- Setters --

-- Update the cell at xyCoords's state to be Visible and then reinsert to the board
revealCell :: Board -> Cell -> Board
revealCell board cell = setCell board cell cell{state=Visible} 

-- Update the cell at xyCoords's state to be Flagged and then reinsert to the board
flagCell :: Board -> XYCors -> Board
flagCell board xyCoords = setCellAt board xyCoords (unsafeGetCellAt board xyCoords){state=Flagged}

-- Update the cell at xyCoords's state to be Questioning and then reinsert to the board
questionCell :: Board -> XYCors -> Board
questionCell board xyCoords = setCellAt board xyCoords (unsafeGetCellAt board xyCoords){state=Questioning}

-- Update the cell at xyCoords's state to be Unknown and then reinsert to the board
hideCell :: Board -> XYCors -> Board
hideCell board xyCoords = setCellAt board xyCoords (unsafeGetCellAt board xyCoords){state=Unknown}

        -- Getters --

isVisible :: Cell -> Bool
isVisible Cell{state=Visible} = True
isVisible _ = False

isUnknown :: Cell -> Bool
isUnknown Cell{state=Unknown} = True
isUnknown _ = False

isFlagged :: Cell -> Bool
isFlagged Cell{state=Flagged} = True
isFlagged _ = False

isQuestioning :: Cell -> Bool
isQuestioning Cell{state=Questioning} = True
isQuestioning _ = False

hasMine :: Cell -> Bool
hasMine Cell{contents=Mine} = True
hasMine _ = False

    -- Mines & Adjacents --

-- returns a list of all the cells with mines on the boards
getAllMines :: Board -> [Cell]
getAllMines [] = []
getAllMines (row:rows) = (filter hasMine row) ++ (getAllMines rows)        -- take only the cells containg mines from the first row and concatenate with the recursion on the tail

-- returns a list of all the cells that are adjacent (horizontally, vertically, or diagonally) to cell0 
getAdjacents :: Board -> Cell -> [Cell]
getAdjacents board cell0 = filter (isAdjacent cell0) (concat board)         -- take only the cells that are adjacent to cell0 from the first row and concatenate with the recursion on the tail

getVisibleAdjacents :: Board -> Cell -> [Cell]
getVisibleAdjacents board cell0 =  filter isVisible (getAdjacents board cell0)

-- returns the number of mines that cell0 has adjacent to it
numAdjMines :: Board -> Cell -> Int
numAdjMines board cell0 = length $ filter (isAdjacent cell0)(getAllMines board)  -- we want the cardinal number of the intersection of the set of all mines and the set of adjacent cells to cell0

{- Blank Areas -}

revealBlankArea :: Board -> XYCors -> Board
revealBlankArea board xyCoords = reveal $ revealCell board (unsafeGetCellAt board xyCoords)

reveal :: Board -> Board
reveal board    | board == forceReveal board = board
                | otherwise = reveal (forceReveal board)

forceReveal :: Board -> Board
forceReveal board = clearCells board (filter (isPartOfABlankArea board) (concat board))

clearCells :: Board -> [Cell] -> Board
clearCells board [] = board
clearCells board (cell:cells) = clearCells (revealCell board cell) cells

isPartOfABlankArea :: Board -> Cell -> Bool
isPartOfABlankArea board cell = any (==0) (map (numAdjMines board) (getVisibleAdjacents board cell)) -- if any visible adjacent cell's are empty then return true
        
{- Detecting Endgame Conditions -}

-- returns true if the cell has a visible mine meaning the cell has caused endgame
isEndCell :: Cell -> Bool
isEndCell cell0 = isVisible cell0 && hasMine cell0

-- returns true if there is a cell in the row with a visible mine meaning the row has caused endgame
isEndRow :: [Cell] -> Bool
isEndRow = any isEndCell

-- returns true if the board has a visible mine causing endgame (searches row by row, cell by cell)
isEndGame :: Board -> Bool
isEndGame = any isEndRow

{- Detecting Winning Conditions -}

-- a row is complete (and therefore not the cause of endgame) if none of its cells are hidden and empty (mineless)
-- a game is complete (and therefore won) if all its rows are complete
isGameComplete :: Board -> Bool
isGameComplete board = length (filter (\cell0 -> (not $ hasMine cell0) && (not $ isVisible cell0)) (concat board)) == 0

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

