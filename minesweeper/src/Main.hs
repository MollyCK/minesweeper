{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

import MinesweeperGame
import OMatic

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

        welcome <- UI.h1 #+ [string "Welcome to Molly's Minesweeper!"]
                         # set style [("text-align", "center")]

        revealModeButton <- UI.button #+ [string "Reveal Mode"]
                                      # set style [
                                                ("padding", "1rem 1.6rem"),
                                                ("border", "1px solid rgb(220,192,155)"),
                                                ("border-radius", "100rem"),
                                                ("color", "inherit"),
                                                ("background-color","hsla(0, 100%, 99%, 0.205)"),
                                                ("transition", ".2s"),
                                                ("cursor", "pointer"),
                                                ("letter-spacing", "2px")
                                      ]

        flagModeButton <- UI.button #+ [string "Flagging Mode"]
                                      # set style [
                                                ("padding", "1rem 1.6rem"),
                                                ("border", "1px solid rgb(220,192,155)"),
                                                ("border-radius", "100rem"),
                                                ("color", "inherit"),
                                                ("background-color","hsla(0, 100%, 99%, 0.205)"),
                                                ("transition", ".2s"),
                                                ("cursor", "pointer"),
                                                ("letter-spacing", "2px")
                                      ]

        questionModeButton <- UI.button #+ [string "Questioning Mode"]
                                      # set style [
                                                ("padding", "1rem 1.6rem"),
                                                ("border", "1px solid rgb(220,192,155)"),
                                                ("border-radius", "100rem"),
                                                ("color", "inherit"),
                                                ("background-color","hsla(0, 100%, 99%, 0.205)"),
                                                ("transition", ".2s"),
                                                ("cursor", "pointer"),
                                                ("letter-spacing", "2px")
                                      ]

        botMoveButton <- UI.button #+ [string "Bot, perform a move!"]
                                      # set style [
                                                ("padding", "1rem 1.6rem"),
                                                ("border", "1px solid rgb(220,192,155)"),
                                                ("border-radius", "100rem"),
                                                ("color", "inherit"),
                                                ("background-color","hsla(0, 100%, 99%, 0.205)"),
                                                ("transition", ".2s"),
                                                ("cursor", "pointer"),
                                                ("letter-spacing", "2px")
                                      ]


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
            -- Translate the coordinates of the mouse on the canvas to be in terms of the cell's coordinates on the board
            translateCoordsMouse = getMinePos <$> UI.mousedown canvas :: Event (Int, Int)

        playMove <- let current = \mode -> case mode of
                                                Mine -> flip revealBlankArea
                                                Flag -> flip flagCell
                                                Uncertain -> flip questionCell
                        in stepper current (current <$ UI.click canvas)

        -- converting drawing mode (what a click of a tile will draw onto that tile) into a behaviour
        -- above and below functions allow for Board-changing events to depend on mouse coordinates and drawing Mode
        drawingMode <- accumB Mine modeToMode

        let
                move :: Event (Board -> Board)
                move = -- Applying translateCoordsMouse event and drawing behavior to playMove
                        let
                                player   = (playMove <*> drawingMode) <@> translateCoordsMouse
                                computer = botMove <$ UI.click botMoveButton
                        in unionWith const player computer         -- applying a mouse coordinates event and drawing behaviour to playMove

        board <- accumB gameBoard move

        liftIO $ onChange board $ \someBoard -> do runUI window (checkPlayerMove window someBoard canvas)

        return ()

-- Translate the coordinates of the mouse down event to be in reference to the mine
getMinePos :: (Int, Int) -> (Int, Int)
getMinePos (x, y) =
    let c = fromIntegral canvasSize :: Float
        w = fromIntegral bWidth     :: Float
        h = fromIntegral bHeight    :: Float
        b = floor $ (fromIntegral x :: Float) / c * w
        a = floor $ (fromIntegral y :: Float) / c * h
    in (a, b)

-- Translate the coordinates of a Cell to be in reference to the given canvas
translateCoordsCell :: (Int, Int) -> (Double, Double)
translateCoordsCell (i, j) = ( (fromIntegral j :: Double) / (fromIntegral bHeight) * (fromIntegral canvasSize) , (fromIntegral i :: Double) / (fromIntegral bWidth)  * (fromIntegral canvasSize)    )

-- using this function to hide the ugly passing of the same board twice in the beginning
drawBoard :: Board -> Element -> UI ()
drawBoard board = drawBoardRecursive (board, board)     

drawBoardRecursive :: (Board, [[Cell]]) -> Element -> UI ()
drawBoardRecursive (_, []) canvas = drawGridLines canvas
drawBoardRecursive (board, (row:rows)) canvas = do
        canvas # set' UI.lineWidth 0.5
        canvas # set' UI.strokeStyle "rgba(135,175,58,255)"
        canvas # set' UI.textFont "11px sans-serif"
        canvas # set' UI.textAlign UI.Center
        drawRow board row canvas
        drawBoardRecursive (board, rows) canvas

drawGridLines :: Element -> UI ()
drawGridLines canvas = do
                        mapM_ (drawLine canvas) (zip top bottom)
                        mapM_ (drawLine canvas) (zip left right)
                        where   boundaries    = [0, 25..(fromIntegral canvasSize)]
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
                                canvas # set' UI.fillStyle (UI.htmlColor colour)
                                canvas # UI.fillRect (cellPosition) 25 25
                                canvas # set' UI.fillStyle  (UI.htmlColor "black")

                                case state cell of
                                        Visible   -> canvas # UI.fillText (toStringCell board cell) relativeTxtPosition
                                        Flagged   -> canvas # UI.fillText "!" relativeTxtPosition
                                        Questioning -> canvas # UI.fillText "?" relativeTxtPosition
                                        _         -> return ()

                                drawRow board cells canvas

                                where cellPosition = translateCoordsCell $ coords cell
                                      relativeTxtPosition = let (x, y) = cellPosition in (x+12, y+14)
                                      colour = getTileColour (state cell)

getTileColour :: VisState -> String
getTileColour Visible = "rgb(220,192,155)"
getTileColour _       = "rgba(191,225,125,255)"

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