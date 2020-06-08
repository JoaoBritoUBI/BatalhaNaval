module GameState where

import Board

-- |Representa o estado total do jogo, i.e. de ambos os tabuleiro
data GameState = GS 
    {
        playerBoard::Board, -- Tabuleiro onde o jogador coloca os seus navios e o computador ataca
        computerBoard::Board, -- Tabuleiro onde o computador coloca os seus navios e o jogador ataca 
        isPlayer::Bool -- Boolean com informação de turnos, se for True joga o jogador, caso contrário joga o computador
    } 

-- |Representa um tabuleiro. 
data Board = Bd 
    {   board :: BoardF, -- Função que representa os valores de cada posição do tabuleio. É obrigatório utilizar uma função
        ships :: [[Coord]] -- Contém inicialmente uma lista com todas as coordenadas de cada navio. Ships está presente para ajudar a determinar quando um navio afunda 

    } deriving Show

-- |Estado inicial
initialState :: GameState
initialState = GS
    { 
        playerBoard = emptyBoard,
        computerBoard = emptyBoard,
        isPlayer = True
    }

emptyBoard :: Board
emptyBoard = Bd {board = checkPosition [[(1,2)],[(2,2)]], ships=[]}

--Definir GameState como instância de Show. Uma possível declaração da BoardF como instância de Show encontra-se definida em Board.hs.
instance Show GameState where
    show game = "\n"
                ++ "|  Player  |\n" ++ show (playerBoard game) ++ "\n\n"
                ++ "| Computer |\n" ++ show (computerBoard game) ++ "\n\n"
                ++ "Player's turn? " ++ show (isPlayer game) ++ "\n"