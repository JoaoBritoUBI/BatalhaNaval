{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board where

import Data.List
import Constants

-- type to represent each position on the board
type Coord = (Int, Int)

-- state of each position
data PositionState = Empty | Success | Fail deriving (Show, Eq)

-- function that maps a "Coord" to a "PositionState"
type BoardF = Coord -> PositionState

-- type that allows "PositionState" lists (i.e. a lines of the board) to have explicit indexes (more user friendly when displaying the board)
type PrettyBoard = (Int,[PositionState])

-- functions that consumes the information of every cell's state and then returns a function that maps "Coord" to "PositionState" based on that information
-- "coordsState" has the structure [ [ (x,y) , ...] , [ (z,a) , ...] ], where: 
-- 1. the first sublist has the cells that HAVE ships and were bombed ("Success");
-- 2. the second sublist has the cells that DON'T HAVE ships and were bombed ("Fail"); 
-- 3. the remaining cells are obviously marked as "Empty"
evalCell :: [[Coord]] -> BoardF
evalCell coordsState coord = if(elem coord (coordsState!!0)) then Success
                             else if(elem coord (coordsState!!1)) then Fail
                             else Empty

-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- IMPLEMENTATION OF SHOW FOR BOARDF
-----------------------------------------------------------------------------------------------------------------------------------------------------------
instance Show BoardF where
    show = showBoard boardSize

-- main function, connects everything together
showBoard :: Int -> BoardF -> String
showBoard size board =
    let listBoard = toPrettyBoard (toList (size-1) board) 0
    in "\n" ++ rowOfNumbers ++ "\n" ++ (concat $ surround (showLine size) (map showRow listBoard))
        where surround x xs = [x] ++ intersperse x xs ++ [x]

-- takes the original list version of the board and turns it into a list version of the board with indexes for each line (so we can print it in a nice way)
toPrettyBoard :: [[PositionState]] -> Int -> [PrettyBoard]
toPrettyBoard listBoard index = if(index/=boardSize) then [(index,listBoard !! index)] ++ (toPrettyBoard listBoard (index+1)) else []

-- creates a string with column numbers
rowOfNumbers :: String
rowOfNumbers = "    " ++ concatMap (++"   ") [show n | n <- [0..(boardSize-1)]]

-- maps a "PositionState" to a showable string
showState :: PositionState -> String
showState Empty    = "   |"
showState Success  = " S |"
showState Fail     = " X |"

-- displays one line of the board
showRow :: PrettyBoard -> String
showRow l = (show (fst l)) ++ " " ++ '|' : concatMap showState (snd l) ++ "\n"

-- displays the top and bottom of one line of the board
showLine :: Int -> String
showLine size = 
    let l = replicate size "---+"
    in "  " ++ '+' : concat l ++ "\n"

-- creates a list version of the board based on the information from the "board" function
toList :: Int -> BoardF -> [[PositionState]]
toList n board =
    let coords = [ [(x,y) | y <- [0..n]] | x <- [0..n] ]
    in map (map board) coords