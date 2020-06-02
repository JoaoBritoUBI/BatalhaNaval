{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board where

import Data.List
import Constants

------------------
--Tipos de dados--
------------------
--Alteracoes devem ser devidamente justificadas.

-- |Representa uma posicao no tabuleiro
type Coord = (Int, Int)

-- |Representa o estado de uma posicao no tabuleiro
data PositionState = Empty | Bombed | Sunken deriving (Show, Eq)

-- |Funcao que associa a cada posicao do tabuleiro um estado
type BoardF = Coord -> PositionState

---------------------------------------
--Implementacao do Show para a BoardF--
---------------------------------------

instance Show BoardF where
    show = showBoard boardSize

showBoard :: Int -> BoardF -> String
showBoard size board =
    let listBoard = toList (size-1) board
    in concat $ surround (showLine size) (map showRow listBoard)
        where surround x xs = [x] ++ intersperse x xs ++ [x]

showState :: PositionState -> String
showState Empty    = "   |"
showState Bombed   = " B |"
showState Sunken   = " S |"

showRow :: [PositionState] -> String
showRow l = '|' : concatMap showState l ++ "\n" 

showLine :: Int -> String
showLine size = 
    let l = replicate size "---+"
    in '+' : concat l ++ "\n"

-- |Cria uma lista de listas que representa a grelha com a informacao proveniente de funcao board
toList :: Int -> BoardF -> [[PositionState]]
toList n board =
    let coords = [ [(x,y) | y <- [0..n]] | x <- [0..n] ]
    in map (map board) coords -- Aplica o board a cada linha