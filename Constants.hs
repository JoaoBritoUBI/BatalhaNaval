-- get ready for some PURE content
module Constants where

----------------------------------------------------------------------------------------------------------------------------------------------------------
-- GAME'S SETTINGS AND CONSTANTS
----------------------------------------------------------------------------------------------------------------------------------------------------------
-- the level of difficulty from the computer
wiseComputer :: Bool
wiseComputer = True

-- the length of each side of the board
boardSize :: Int
boardSize = 10

-- the radius for the computer to search in, centered around any position where a player's ship was found
searchRadius :: Int
searchRadius = 1

-- the length of every ship available
shipSizes :: [Int]
shipSizes = [5,4,4,3,3,3,2,2,2,2]

-- the names of every ship available
shipNames :: [[Char]]
shipNames = ["Aircraft Carrier","Tank Ship","Tank Ship","Destroyer","Destroyer","Destroyer","Submarine","Submarine","Submarine","Submarine"]

-- just for testing purposes
testInputs :: [String]
testInputs = ["(1,1);(1,5)","(2,2);(5,2)","(3,4);(3,7)","(2,3);(2,5)","(5,4);(7,4)","(4,7);(6,7)","(4,0);(4,1)","(7,1);(7,2)","(2,0);(3,0)","(0,7);(1,7)"]