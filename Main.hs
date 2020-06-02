import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Char

import System.Random

import GameState
import Board
import Constants

-------------------------------------------------------------
-- Implementar neste ficheiro todo o código que não é puro
-------------------------------------------------------------
main :: IO ()
main = do
		putStr "Code"

-- gerar coordenadas random
getRandomCoordinates :: IO Coord
getRandomCoordinates = do
    x <- randomIO
    y <- randomIO
    return (x `mod` boardSize, y `mod` boardSize)