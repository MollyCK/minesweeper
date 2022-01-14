module OMatic
    ( botMove
    ) where

import MinesweeperGame
import Data.List

 {- Choosing Moves -}

botMove :: Board -> Board
botMove board = case (filter (not . isFlagged) (getDefiniteMines board)) of    -- a list of unflagged definite Mines
                    (y:ys) -> flagCell board (coords y)
                    []     -> case (getAdjacentEmpties board (getDefiniteMines board)) of     -- there are no unflagged definite Mines so we can...
                                (x:_) -> revealBlankArea board (coords x)               -- start revealing cells and see if that helps us in identifying some more definite Mines later on
                                []    -> if not $ isGameComplete board                  
                                            then revealBlankArea board (coords $ makeEducatedGuess board)
                                         else board

-- This is the last resort when we cannot identify any definites, we make an guess (educated on probability of hitting a mine : the bigger the number of adjacents' adjacents, the more likely to hit a mine)
makeEducatedGuess :: Board -> Cell
makeEducatedGuess board = case filter isVisible (concat board) of
                            [] -> unsafeGetCellAt board ((length board) `div` 2, (length board) `div` 2)    -- if nothing is visible on the board then revealing the center cell is the best chance of a large opening
                            visibles -> head (sortOn (numAdjAdj board) adjHidden)  -- sort the list of hidden adjacents by each of their adjacents' adjacents in ascending order and take the one with the least likelihood of hitting a mine
                                        where adjacents = nub $ concatMap (\cell0 -> filter (isAdjacent cell0) (concat board)) visibles       -- careful to remove duplicates with nub
                                              adjHidden = (filter (not . isFlagged) adjacents) \\ visibles

 {- Identifying Definites -}

        -- Mines --

-- Based on what is currently visible on the board, the bot attempts to identify cells that definitely contain Mines
-- At the beginning, when there is nothing visible, the bot will reveal the centre right cell and go from there, moving NorthWest as long as probability is on its side until it can accurately identify a Mine cell
getDefiniteMines :: Board -> [Cell]
getDefiniteMines board = getMinesAdjacent board []      -- getMinesAdjacent is a recursive function that updates a list at each level of recursion so we first pass in an empty list as we do not know of any definite mines yet

--returns a list of cells that are definitely mines because of the cells they are directly adjacent to
getMinesAdjacent :: Board -> [Cell] -> [Cell]
getMinesAdjacent board cells    | cells == (runGetMinesAdjacent board cells)   = (runGetMinesAdjacent board cells)      -- if there are no more definite mines to be found, return as is
                                | otherwise = getMinesAdjacent board (runGetMinesAdjacent board cells)

-- returns a list of cells that are definite mines because they are adjacent to visible cells with no other options for mine placement 
runGetMinesAdjacent :: Board -> [Cell] -> [Cell]
runGetMinesAdjacent board mines = foldl (definiteAdjacentMines board) mines (concat board)

-- returns a list of mines that are definite mines because they are adjacent to visible cells with no other options for mine placement. 
-- Takes into account visible empty cells and definite Mine cells that have already been flagged
-- Note* the number of mines left adjacent to the given are cell is (all adjacent mines) - (all definite mines so far)
definiteAdjacentMines :: Board -> [Cell] -> Cell -> [Cell]
definiteAdjacentMines board defMines cell = if length unknown == (numAdjMines board cell) - (length hidden - length unknown)   -- (Note*) if the number of unknowns is the same as the number of cells left adjacent to the given cell
                                                then defMines ++ unknown
                                            else defMines
                                                where hidden    = getHiddenAdj board cell    -- getting all the hidden cells adjacent to the given cell
                                                      unknown   = hidden \\ defMines      -- unknown cells are those that are hidden and not definite mines (that we know of so far)

        -- Empties --

-- returns a list of cells that are definitely empty because of the cells they are directly adjacent to
getAdjacentEmpties :: Board -> [Cell] -> [Cell]
getAdjacentEmpties board mines = concatMap (definiteAdjacentEmpties board mines) (filter isVisible (concat board))

-- returns a list of empty cells that are definitely empty because their adjacent cells have all their mines accounted for
definiteAdjacentEmpties :: Board -> [Cell] -> Cell -> [Cell]
definiteAdjacentEmpties board mines cell = if (length hidden - length unknown) == (numAdjMines board cell)  -- if the number of hidden definite cells is the same as the number of mines adjacent to the given cell then we have found all the mines for this cell so...
                                                then unknown        -- then all the non-definite hidden cells (unknowns) have to be definite empties
                                            else []     -- there are no empties left
                                            where hidden    = getHiddenAdj board cell -- getting all the hidden cells adjacent to the given cell
                                                  unknown   = hidden \\ mines     -- unknown cells are those that are hidden and not definite mines (that we know of so far)

 {- Utility Functions -}
 
 -- returns a list of all the cells that are directly adjacent to the given cell and not visible
getHiddenAdj :: Board -> Cell -> [Cell]
getHiddenAdj board cell0 = filter (\cell1 -> isAdjacent cell0 cell1 && not (isVisible cell1)) (concat board)

--returns a list of all the visible cells that are directly adjacent to the given cell
getVisibleAdj :: Board -> Cell -> [Cell]
getVisibleAdj board cell0 = filter (\cell1 -> isVisible cell1 && isAdjacent cell0 cell1) (concat board)  -- creating a list of cells that are visible and adjacent to the given cell

-- returns the total number of mines that the given cell's visible adjacent cells are adjacent to (the number of adjacents to the adjacents)
numAdjAdj :: Board -> Cell -> Int
numAdjAdj board cell0 = sum $ map (numAdjMines board) (getVisibleAdj board cell0) -- sum all the numbers that cell0 touches to get the total number of mines that cell0's adjacent cells are adjacent to
