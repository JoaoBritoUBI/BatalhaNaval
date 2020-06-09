module Constants where

-- the length of each side of the board
boardSize :: Int
boardSize = 10

-- the length of every ship available
shipSizes :: [Int]
shipSizes = [5,4,4,3,3,3,2,2,2,2]

-- the names of every ship available
shipNames :: [[Char]]
shipNames = ["Porta-Aviões","Navio-Tanque","Navio-Tanque","Contratorpedeiro","Contratorpedeiro","Contratorpedeiro","Submarino","Submarino","Submarino","Submarino"]

-- the amount of ships that can be placed on the board
numShips :: Int
numShips = length shipNames

-- just for testing purposes
testInputs :: [String]
testInputs = ["(1,1);(1,5)","(2,2);(5,2)","(3,4);(3,7)","(2,3);(2,5)","(5,4);(7,4)","(4,7);(6,7)","(4,0);(4,1)","(7,1);(7,2)","(2,0);(3,0)","(0,7);(1,7)"]