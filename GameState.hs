module GameState where

import Board

-- |Representa o estado total do jogo, i.e. de ambos os tabuleiro
-- playerBoard: Tabuleiro onde o jogador coloca os seus navios e o computador ataca
-- computerBoard: Tabuleiro onde o computador coloca os seus navios e o jogador ataca 
data GameState = GS 
    {
        --Implementar

    } 

-- |Representa um tabuleiro. 
-- board: Funcao que representa os valores de cada posicao do tabuleio. E' obrigatorio utilizar uma funcao
-- ships: Contem inicialmente uma lista com todas as coordenadas de cada navio. ships esta' presente para ajudar a determinar quando um navio afunda.  
data Board = Bd 
    {   board :: BoardF, 
        ships :: [[Coord]] 

    } deriving Show


-- |Estado inicial
initialState :: GameState
initialState = GS
    { 
        -- declarar o estado inicial
    }


emptyBoard :: Board
emptyBoard = Bd {board = const Empty, ships=[]}


--Definir GameState como instancia de Show. Uma possivel declaracao da BoardF como instancia de Show encontra-se definida em Board.hs.
--instance Show GameState where