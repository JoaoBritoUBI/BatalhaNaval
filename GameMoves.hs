-- contains IMPURE content
module GameMoves where

import Board
import AuxFunctions
import System.Random
import Constants
import Text.Read hiding (get)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY FUNCTIONS RELATED TO THE COMPUTER'S AND PLAYER'S MOVES (IMPURE)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- generates pseudo-random coordinates
getRandomCoordinates :: IO Coord
getRandomCoordinates = do
    x <- randomIO
    y <- randomIO
    return (x `mod` boardSize, y `mod` boardSize)

-- get a coordinate for the player's attack
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
getComputerMove :: [Coord] -> IO Coord
getComputerMove alreadyChosen = do if(not wiseComputer) then -- pseudo-randomly choose a coordinate for the computer's attack
                                    do move <- getRandomCoordinates
                                       if(elem move alreadyChosen) then getComputerMove alreadyChosen
                                       else return move
                                   
                                   else -- wisely choose a coordinate for the computer's attack
                                       do return (-1,-1) -- TODO

-- chooses where to place the player's ships
initializePlayerShips :: Int -> [[Coord]] -> IO [[Coord]]
initializePlayerShips currentShip finalList = do 
                                                if(currentShip/=numShips) then do
                                                    -- TEST VERSION
                                                    --let n = testInputs !! currentShip
                                                    
                                                    -- FINAL VERSION
                                                    putStr ("\n" ++ (shipNames !! currentShip) ++ " | len = " ++ show (shipSizes !! currentShip) ++ " > ")
                                                    -- read and check the input
                                                    n <-  getLine

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
initializeComputerShips currentShip finalList = do if(not wiseComputer) then -- pseudo-randomly choose where to place the computer's ships
                                                    do if(currentShip/=numShips) then do

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

                                                   else do -- wisely choose where to place the computer's ships
                                                    return [[(-1,-1)]] -- TODO