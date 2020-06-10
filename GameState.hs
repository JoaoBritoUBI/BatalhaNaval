module GameState where

import Board

-- the state of the game (i.e. both boards)
data GameState = GS 
    {   
        -- player's boards
        playerDefenseBoard::Board, -- the board where the player has its ships and the computer attacks
        playerOffenseBoard::Board, -- the board where the player attacks, without knowing where the computer's ships are
        playerMoves::[Coord],

        -- computer's boards
        computerDefenseBoard::Board, -- the board where the computer has its ships and the player attacks
        computerOffenseBoard::Board, -- the board where the computer attacks, without knowing where the player's ships are
        computerMoves::[Coord]
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

        -- computer's boards
        computerDefenseBoard = emptyBoard,
        computerOffenseBoard = emptyBoard,
        computerMoves = []
    }

-- the starter board (i.e. all cells are empty)
emptyBoard :: Board
emptyBoard = Bd {board = const Empty, ships=[]}

-- instance of Show for GameState
instance Show GameState where
    show game = "\n"
                ++ "|  Player's Ships  |\n" ++ show (playerDefenseBoard game) ++ "\n\n"
                ++ "| Computer's Ships |\n" ++ show (computerDefenseBoard game) ++ "\n\n"