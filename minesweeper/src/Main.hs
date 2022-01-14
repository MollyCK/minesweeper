{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

import           Data.IORef
import           Control.Monad.Trans (liftIO)

import Data.Maybe

import MinesweeperGame
import Graphics.UI.Threepenny (canvas)

bHeight = 20 :: Int
bWidth = 20 :: Int
canvasSize = 25*bHeight :: Int

data Mode = Mine | Flag | Uncertain deriving Show

main :: IO ()
main = startGUI defaultConfig runGame

runGame :: Window -> UI ()
runGame window = do 
        return window # set title "Molly's Minesweeper Game"    -- sets the title for the tab in a browser
        
        -- Taken from Lecture Slides 9.02: slide 14
        canvas <- UI.canvas
                # set UI.height canvasSize
                # set UI.width canvasSize
                # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

        gameBoard <- liftIO $ generateBoard bHeight bWidth
        drawBoard gameBoard canvas

        revealModeButton <- UI.button #+ [string "reveal cell"]
        flagModeButton <- UI.button #+ [string "flag cell"]
        questionModeButton <- UI.button #+ [string "mark questioning"]
        botMoveButton <- UI.button #+ [string "Bot perform a move!"]

        getBody window #+ [
                column [element canvas],
                element revealModeButton,
                element flagModeButton,
                element questionModeButton,
                element botMoveButton
         ]

        -- a <$ bs is the same as do bs \\ pure a (Source: https://hackage.haskell.org/package/threepenny-gui-0.9.1.0/docs/Graphics-UI-Threepenny-Core.html#)
        -- below functions allow for updating the current Mode after a click on a respective button
        let modeToMine = const Mine <$ UI.click revealModeButton :: Event (Mode -> Mode)
            modeToFlag = const Flag <$ UI.click flagModeButton :: Event (Mode -> Mode)
            modeToQuestioning = const Uncertain <$ UI.click questionModeButton :: Event (Mode -> Mode)
            -- creating an Event stream that fires when any of the three "Mode update" buttons are clicked
            modeToMode = unionWith const modeToMine (unionWith const modeToFlag modeToQuestioning) :: Event (Mode -> Mode)
            coord :: Event (Int, Int)
            coord = getMinePos <$> UI.mousedown canvas

        performMove <- let current = \mode -> case mode of
                                                Mine -> flip revealBlankArea
                                                Flag -> flip flagCell
                                                Uncertain -> flip questionCell
                        in stepper current (current <$ UI.click canvas) 

        -- converting drawing mode (what a click of a tile will draw onto that tile) into a behaviour
        -- above and below functions allow for Board-changing events to depend on mouse coordinates and drawing Mode
        drawingMode <- accumB Mine modeToMode

        -- performMove <*> drawingMode <@> coord
        let
                move :: Event (Board -> Board)
                move = -- Applying coord event and digging behavior to makeMove
                        let
                            player   = (performMove <*> drawingMode) <@> coord
                            computer = botMove <$ UI.click botMoveButton

                        in unionWith const player computer         -- applying a mouse coordinates event and drawing behaviour to performMove

        board <- accumB gameBoard move

        liftIO $ onChange board $ \someBoard -> do runUI window (checkPlayerMove window someBoard canvas)

        return ()

-- DANGER
botMove :: Board -> Board
botMove _ = [[]]

getMinePos :: (Double, Double) -> (Int, Int)
getMinePos (x, y) =
    let c = fromIntegral canvasSize :: Float
        w = fromIntegral bWidth     :: Float
        h = fromIntegral bHeight    :: Float
        b = floor $ (realToFrac x :: Float) / c * w     -- CAUTION realToFrac
        a = floor $ (realToFrac y :: Float) / c * h
    in (a, b)

-- Translate the coordinates of a Cell to be in reference to the given canvas
translateCoordsCell :: (Int, Int) -> (Double, Double)
translateCoordsCell (i, j) = (x, y)
                        where x = (fromIntegral j :: Double) / ((fromIntegral bHeight :: Double) * (fromIntegral canvasSize  :: Double))        -- Specifying Double everywhere to ensure Double division and that nothing gets cutoff
                              y = (fromIntegral i :: Double) / ((fromIntegral bWidth :: Double) * (fromIntegral canvasSize  :: Double))

                     
-- using this function to hide the ugly passing of the same board twice in the beginning                        
drawBoard :: Board -> Element -> UI ()
drawBoard board = drawBoardRecursive (board, board)     

drawBoardRecursive :: (Board, [[Cell]]) -> Element -> UI ()
drawBoardRecursive (_, []) canvas = drawGridLines canvas
drawBoardRecursive (board, (row:rows)) canvas = do
        canvas # set' UI.lineWidth 0.5
        canvas # set' UI.strokeStyle "brown"
        canvas # set' UI.textFont "11px sans-serif"
        canvas # set' UI.textAlign UI.Center
        drawRow board row canvas
        drawBoardRecursive (board, rows) canvas

drawGridLines :: Element -> UI ()
drawGridLines canvas = do
                        mapM_ (drawLine canvas) (zip top bottom)
                        mapM_ (drawLine canvas) (zip left right)
                        where   boundaries    = [0, 20..(fromIntegral canvasSize)]
                                top       = zip (repeat 0) boundaries
                                bottom    = zip (repeat (fromIntegral canvasSize)) boundaries
                                left      = zip boundaries (repeat 0)
                                right     = zip boundaries (repeat (fromIntegral canvasSize))

drawLine :: Element -> (UI.Point, UI.Point) -> UI ()
drawLine canvas (x, y) = do
                        UI.beginPath canvas
                        UI.moveTo x canvas
                        UI.lineTo y canvas
                        UI.closePath canvas
                        UI.stroke canvas

drawRow :: Board -> [Cell] -> Element -> UI()
drawRow _ [] _ = return ()
drawRow board (cell:cells) canvas = do
                                canvas # set' UI.fillStyle (UI.htmlColor "Peach")
                                canvas # UI.fillRect (cellPosition) 20 20
                                canvas # set' UI.fillStyle  (UI.htmlColor "Charcoal")

                                canvas # UI.fillText (toStringCell board cell) relativeTxtPosition

                                drawRow board cells canvas

                                where cellPosition = translateCoordsCell $ coords cell
                                      relativeTxtPosition = let (x, y) = cellPosition in (x+10, y+10)

checkPlayerMove :: Window -> Board -> Element -> UI ()
checkPlayerMove window board canvas = do
                                        drawBoard board canvas
                                        if (isEndGame board) then displayLoser window
                                        else if (isGameComplete board) then displayWinner window
                                        else return ()

displayLoser :: Window -> UI ()
displayLoser window = do 
                msg <- UI.h3 #+ [string "Oh no, you hit a mine! ...That's game over!"]
                getBody window #+ [element msg]
                return ()

displayWinner :: Window -> UI ()
displayWinner window = do
                        msg <- UI.h3 #+ [string "The game is complete. Congratulations, you are a winner!"]
                        getBody window #+ [element msg]
                        return ()