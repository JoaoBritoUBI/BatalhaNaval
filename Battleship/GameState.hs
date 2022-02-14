-- get ready for some PURE content
module GameState where

import Board
import Constants

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- GAME'S BOARD RECORD AND GAMESTATE DECLARATION
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- the state of the game (i.e. boards and moves)
data GameState = GS 
    {   
        -- player's boards
        playerDefenseBoard::Board, -- the board where the player has its ships and the computer attacks
        playerOffenseBoard::Board, -- the board where the player attacks, without knowing where the computer's ships are
        playerMoves::[Coord], -- the history of moves by the player
        -- BEGIN TEST VERSION 
        --playerPriorityStack::[Coord], -- the player's priority stack
        -- END TEST VERSION

        -- computer's boards
        computerDefenseBoard::Board, -- the board where the computer has its ships and the player attacks
        computerOffenseBoard::Board, -- the board where the computer attacks, without knowing where the player's ships are
        computerMoves::[Coord], -- the history of moves by the computer
        computerPriorityStack::[Coord] -- the computer's priority stack
    } 

-- the representation of a single board
data Board = Bd 
    {   board :: BoardF, -- function that maps each "Coord" into a "PositionState"
        ships :: [[Coord]] -- list of lists with the coordinates of every ship (obviously, the "playerOffenseBoard" and "computerOffenseBoard" will have an empty list)

    } deriving Show

-- the game's initial state
initialState :: GameState
initialState = GS
    { 
        -- player's boards
        playerDefenseBoard = emptyBoard,
        playerOffenseBoard = emptyBoard,
        playerMoves = [],
        -- BEGIN TEST VERSION 
        --playerPriorityStack = [],
        -- END TEST VERSION

        -- computer's boards
        computerDefenseBoard = emptyBoard,
        computerOffenseBoard = emptyBoard,
        computerMoves = [],
        computerPriorityStack = []
    }

-- the starter board (i.e. all cells are empty)
emptyBoard :: Board
emptyBoard = Bd {board = const Empty, ships=[]}

-- instance of Show for GameState
instance Show GameState where
    show game = "\n"
                ++ "   " ++ (replicate (((5+(4*(boardSize-1))) - 30)`div`2) ' ') ++ "Player's Defense Board + Ships" ++ "\n" ++ show (playerDefenseBoard game) ++ "\n\n"
                ++ "   " ++ (replicate (((5+(4*(boardSize-1))) - 32)`div`2) ' ') ++ "Computer's Defense Board + Ships" ++ "\n" ++ show (computerDefenseBoard game) ++ "\n\n"