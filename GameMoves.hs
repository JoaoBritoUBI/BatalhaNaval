-- get ready for some PURE and IMPURE content
module GameMoves where

import Board
import AuxFunctions
import System.Random
import Constants
import Text.Read hiding (get)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY FUNCTIONS RELATED TO THE COMPUTER'S MOVES (PURE)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- returns every even line in a typical checkerboard
evenCheckerboard :: Int -> [(Int,Int)]
evenCheckerboard y = [(y,x) | x <- [1..(boardSize-1)], odd x]

-- returns every odd line in a typical checkerboard
oddCheckerboard :: Int -> [(Int,Int)]
oddCheckerboard y = [(y,x) | x <- [0..(boardSize-2)], even x]

-- if we imagine the board as a checkerboard, this function returns every black square (alternates between "evenCheckerboard" and "oddCheckerboard")
masterCheckerboardBlack :: Int -> [(Int,Int)] -> [(Int,Int)]
masterCheckerboardBlack y finalList = if(y==boardSize) then finalList
                                 else if(even y) then (masterCheckerboardBlack (y+1) (finalList ++ (evenCheckerboard y))) else (masterCheckerboardBlack (y+1) (finalList ++ (oddCheckerboard y)))

-- if we imagine the board as a checkerboard, this function returns every white square (alternates between "oddCheckerboard" and "evenCheckerboard")
masterCheckerboardWhite :: Int -> [(Int,Int)] -> [(Int,Int)]
masterCheckerboardWhite y finalList = if(y==boardSize) then finalList
                                 else if(even y) then (masterCheckerboardWhite (y+1) (finalList ++ (oddCheckerboard y))) else (masterCheckerboardWhite (y+1) (finalList ++ (evenCheckerboard y)))

-- returns a search area for the computer to exploit
getSearchArea :: Coord -> [Coord] -> [Coord] -> Int -> [[Coord]]
getSearchArea coord alreadyVisited all currentDistance = if(currentDistance>searchRadius) then [] -- stop criterium
                                                         else do
                                                            if(all==[]) then do 
                                                                    let aux = filter (\x -> (abs ((fst x)-(fst coord)))<=searchRadius && (abs ((snd x)-(snd coord)))<=searchRadius && not (elem x alreadyVisited)) [ (y,x) | y <- [0..(boardSize-1)], x <- [0..(boardSize-1)]]
                                                                    getSearchArea coord alreadyVisited aux currentDistance
                                                            else do
                                                                let subGroup = filter (\x ->(abs ((fst x)-(fst coord)))<=currentDistance && (abs ((snd x)-(snd coord)))<=currentDistance) all
                                                                ([subGroup] ++ (getSearchArea coord alreadyVisited all (currentDistance+1)))

-- checks if the search area has been fully explored or not
exhaustedSearchArea :: [Coord] -> [[Coord]] -> Bool
exhaustedSearchArea alreadyVisited searchArea = (filter (\x -> not (elem x alreadyVisited)) (concat searchArea))==[]

-- returns the sub-area that is being exploited
getCurrentSubArea :: [[Coord]] -> [Coord] -> [Coord]
getCurrentSubArea [] _ = []
getCurrentSubArea (x:xs) alreadyVisited = do let coordsLeft = (filter (\a -> not (elem a alreadyVisited)) x)
                                             if(coordsLeft/=[]) then coordsLeft
                                             else getCurrentSubArea xs alreadyVisited

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY FUNCTIONS RELATED TO THE COMPUTER AND PLAYER'S MOVES (IMPURE)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- generates pseudo-random coordinates
getRandomCoordinates :: IO Coord
getRandomCoordinates = do
    y <- randomIO
    x <- randomIO
    return (y `mod` boardSize, x `mod` boardSize)

-- generates pseudo-random integers
getRandomIndex :: Int -> IO Int
getRandomIndex len = do
    i <- randomIO
    return (i `mod` len)

-- retrieves a coordinate for the player's attack
getPlayerMove :: [Coord] -> IO Coord
getPlayerMove alreadyChosen = do n <- getLine
                                 if(n=="q") then return (-1,-1) -- leave the game
                                 else do
                                    let aux = readMaybe n :: Maybe Coord

                                    -- the coordinate in in the wrong format
                                    if(aux==Nothing) then do
                                        putStr "(Player) Wrong format! Try again > "
                                        getPlayerMove alreadyChosen
                                    
                                    -- the coordinate was already chosen
                                    else do
                                        if(elem (fromJust aux) alreadyChosen) then do
                                            putStr "(Player) Already chosen! Try again > "
                                            getPlayerMove alreadyChosen
                                        
                                        -- properly formated coordinate and not already chosen
                                        else do
                                            -- the coordinate doesn't fit inside the board
                                            if(not (properCoord (fromJust aux))) then do
                                                putStr "(Player) Doesn't fit inside the board! Try again > "
                                                getPlayerMove alreadyChosen
                                            
                                            -- proper coordinate
                                            else return (fromJust aux)

-- chooses a coordinate for the computer's attack
getComputerMove :: [Coord] -> [Coord] -> [Coord] -> IO Coord
getComputerMove alreadyChosen checkerboard computerExploitSubArea = do if(not wiseComputer) then do -- pseudo-randomly choose a coordinate for the computer's attack
                                                                        move <- getRandomCoordinates
                                                                        if(elem move alreadyChosen) then (getComputerMove alreadyChosen checkerboard [])
                                                                        else return move
                                                            
                                                                       else -- wisely choose a coordinate for the computer's attack
                                                                        do if(computerExploitSubArea/=[]) then do -- the computer has found a ship, let's explore the surrounding area and try to sink it
                                                                            index <- (getRandomIndex (length computerExploitSubArea))
                                                                            if(elem (computerExploitSubArea !! index) alreadyChosen) then (getComputerMove alreadyChosen checkerboard computerExploitSubArea)
                                                                            else return (computerExploitSubArea !! index)
                                                                            
                                                                            else do -- the computer has not found any ship, it will guess at random (using the checkerboard method)
                                                                                index <- (getRandomIndex (length checkerboard))
                                                                                return (checkerboard !! index)

-- chooses where to place the player's ships
initializePlayerShips :: Int -> [[Coord]] -> IO [[Coord]]
initializePlayerShips currentShip finalList = do 
                                                if(currentShip/=numShips) then do
                                                    -- BEGIN TEST VERSION
                                                    --let n = testInputs !! currentShip
                                                    -- END TEST VERSION
                                                    
                                                    -- BEGIN FINAL VERSION
                                                    
                                                    putStr ("\n" ++ (shipNames !! currentShip) ++ " | len = " ++ show (shipSizes !! currentShip) ++ " > ")
                                                    -- read and check the input
                                                    n <-  getLine

                                                    -- BEGIN FINAL VERSION

                                                    if(n=="q") then return [[(-1,-1)]] -- leave the game
                                                    else do
                                                        -- get the actual coordinates from the input string 
                                                        let coords = parseInput n

                                                        -- wrong format
                                                        if(coords==[(-1,-1)]) then do
                                                            putStrLn "(Player) Wrong format! Try again!"
                                                            initializePlayerShips currentShip finalList

                                                        else do
                                                            -- check if the coordinates have the right size
                                                            if((getShipSize (coords !! 0) (coords !! 1))==(shipSizes !! currentShip)) then do

                                                                -- compute the intermediate coordinates
                                                                let newShipCoords = (getIntermediateCoords (coords !! 0) (coords !! 1))

                                                                -- check if the coordinates don't extend beyond the boards' limits or overlap with other ships
                                                                if(checkCoords newShipCoords finalList) then do

                                                                    -- move to the next one
                                                                    initializePlayerShips (currentShip+1) (finalList ++ [newShipCoords])

                                                                else do 
                                                                    putStrLn "(Player) The ship extends beyond the board! Try again!"
                                                                    initializePlayerShips currentShip finalList

                                                            else do
                                                                putStrLn "(Player) The ship doesn't have the correct size! Try again!"
                                                                initializePlayerShips currentShip finalList
                                                
                                                else return finalList

-- chooses where to place the computer's ships
initializeComputerShips :: Int -> [[Coord]] -> IO [[Coord]]
initializeComputerShips currentShip finalList = do if(currentShip/=numShips) then do -- pseudo-randomly choose where to place the computer's ships

                                                        startCoord <- getRandomCoordinates
                                                    
                                                        -- try to position a ship with size "currentShip" starting from coordinates "coord"
                                                        let newShipCoords = (getShipCoords startCoord (shipSizes !! currentShip) finalList)
                                                        
                                                        -- there is space for the new ship
                                                        if((length newShipCoords)/=0) then do

                                                            -- move to the next one
                                                            initializeComputerShips (currentShip+1) (finalList ++ [newShipCoords])
                                                        
                                                        -- try again
                                                        else initializeComputerShips currentShip finalList
                                                    
                                                    else return finalList