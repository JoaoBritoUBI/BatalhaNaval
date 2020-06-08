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
data PositionState = Empty | Success | Fail deriving (Show, Eq)

-- |Funcao que associa a cada posicao do tabuleiro um estado
type BoardF = Coord -> PositionState

type PrettyBoard = (Int,[PositionState])

-- função que verifica o estado de cada posição
checkPosition :: [[Coord]] -> BoardF
checkPosition coordsState coord = if(elem coord (coordsState!!0)) then Success
                                  else if(elem coord (coordsState!!1)) then Fail
                                  else Empty

---------------------------------------
--Implementacao do Show para a BoardF--
---------------------------------------
instance Show BoardF where
    show = showBoard boardSize

toPrettyBoard :: [[PositionState]] -> Int -> [PrettyBoard]
toPrettyBoard listBoard index = if(index/=boardSize) then [(index,listBoard !! index)] ++ (toPrettyBoard listBoard (index+1)) else []

rowOfNumbers :: String
rowOfNumbers = "    " ++ concatMap (++"   ") [show n | n <- [0..(boardSize-1)]]

showBoard :: Int -> BoardF -> String
showBoard size board =
    let listBoard = toPrettyBoard (toList (size-1) board) 0
    in "\n" ++ rowOfNumbers ++ "\n" ++ (concat $ surround (showLine size) (map showRow listBoard))
        where surround x xs = [x] ++ intersperse x xs ++ [x]

showState :: PositionState -> String
showState Empty    = "   |"
showState Success  = " S |"
showState Fail     = " X |"

showRow :: PrettyBoard -> String
showRow l = (show (fst l)) ++ " " ++ '|' : concatMap showState (snd l) ++ "\n"

showLine :: Int -> String
showLine size = 
    let l = replicate size "---+"
    in "  " ++ '+' : concat l ++ "\n"

-- |Cria uma lista de listas que representa a grelha com a informacao proveniente de funcao board
toList :: Int -> BoardF -> [[PositionState]]
toList n board =
    let coords = [ [(x,y) | y <- [0..n]] | x <- [0..n] ]
    in map (map board) coords -- Aplica o board a cada linha