module GameState where

import Board

-- the state of the game (i.e. both boards)
data GameState = GS 
    {
        playerBoard::Board, -- player's board (where the computer attacks)
        computerBoard::Board, -- computer's board (where the player attacks)
        isPlayer::Bool -- boolean value to indicate whose turn it is ("True" if it is the player's turn, "False" otherwise)
    } 

-- the representation of a single board
data Board = Bd 
    {   board :: BoardF, -- function that maps each "Coord" into a "PositionState"
        ships :: [[Coord]] -- list of lists with the coordinates of every ship

    } deriving Show

-- the game's initial state
initialState :: GameState
initialState = GS
    { 
        playerBoard = emptyBoard,
        computerBoard = emptyBoard,
        isPlayer = True
    }

-- the starter board (i.e. all cells are empty)
emptyBoard :: Board
emptyBoard = Bd {board = const Empty, ships=[]}

-- instance of Show for GameState
instance Show GameState where
    show game = "\n"
                ++ "|  Player  |\n" ++ show (playerBoard game) ++ "\n\n"
                ++ "| Computer |\n" ++ show (computerBoard game) ++ "\n\n"
                ++ "Player's turn? " ++ show (isPlayer game) ++ "\n"