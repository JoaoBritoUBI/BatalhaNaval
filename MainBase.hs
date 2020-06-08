import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Exception
import Data.Char
import Data.List.Split

import System.Random

import GameState
import Board
import Constants

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY FUNCTIONS
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- parses the input string and converts it into "Coord" format
parseInput :: String -> [Coord]
parseInput rawCoords = map (\[x,y] -> (x,y)) (map (\x -> map (\y -> read y :: Int) (splitOn "," x)) (splitOn ";" [x | x <- rawCoords, not (elem x "()")]))

-- returns the size of a ship starting in "startCoord" and finishing in "endCoord"
getShipSize :: Coord -> Coord -> Int
getShipSize startCoord endCoord = (abs ((fst startCoord)-(fst endCoord))) + (abs ((snd startCoord)-(snd endCoord))) + 1

-- checks if we can place a ship within the given coordinates
checkCoords :: Coord -> Coord -> [[Coord]] -> Int -> Bool
checkCoords startCoord endCoord otherShips shipSize = startCoord>=(0,0) && endCoord>=(0,0) -- check if the ship extends outside of the boards' limits
                                             && not (elem True (map (elem startCoord) otherShips)) && not (elem True (map (elem endCoord) otherShips)) -- check if either the starting or finishing positions overlap with other ships

-- computes the missing coordinates between "startCoord" and "endCoord"
getIntermediateCoords :: Coord -> Coord -> [Coord]
getIntermediateCoords startCoord endCoord = [(a,b) | a <- [(fst startCoord)..(fst endCoord)], b <- [(snd startCoord)..(snd endCoord)]]

-- generates random coordinates
getRandomCoordinates :: IO Coord
getRandomCoordinates = do
    x <- randomIO
    y <- randomIO
    return (x `mod` boardSize, y `mod` boardSize)

-- checks if the starting position can be used
isStartOk :: Coord -> [[Coord]] -> Bool
isStartOk startCoord otherShips = not (elem True (map (elem startCoord) otherShips))

-- attempts to position a ship with size "shipSize" starting from coordinates "startCoord"
getShipCoords :: Coord -> Int -> [[Coord]] -> [Coord]
getShipCoords startCoord shipSize otherShips = do 
                                                -- filter out impossible positions (i.e. outside the boards' limits) from the starting one
                                                let possiblePositions = filter (\(a,b) -> a>=0 && b>=0) [((fst startCoord)+(fst c),(snd startCoord)+(snd c)) | c <- [(-(shipSize-1),0),(0,(shipSize-1)),((shipSize-1),0),(0,-(shipSize-1))]]
                                                
                                                -- filter out the positions that would overlap with other ships
                                                let possiblePositionsAux = [p | p <- possiblePositions, not (elem True (map (elem p) otherShips))]

                                                if((length possiblePositionsAux)==0) then []
                                                else do
                                                    -- choose one of the available positions
                                                    let endCoord = possiblePositionsAux !! 0
                                                    
                                                    -- get the intermediate coordenates
                                                    if(startCoord>endCoord) then
                                                        reverse (getIntermediateCoords startCoord endCoord)
                                                    else (getIntermediateCoords startCoord endCoord)

-----------------------------------------------------------------------------------------------------------------------------------------------
-- MAIN FUNCTIONS
-----------------------------------------------------------------------------------------------------------------------------------------------
-- initialize computer board
initializeComputerBoard :: Int -> StateT GameState IO()
initializeComputerBoard currentShip = do state <- get
                                         if(currentShip/=numShips) then do

                                            startCoord <- liftIO$getRandomCoordinates
                                            let otherShips = (ships (computerBoard state))
                                            
                                            -- try to position a ship with size "currentShip" starting from coordinates "coord"
                                            if(isStartOk startCoord otherShips) then do
                                            
                                                let newShipCoords = getShipCoords startCoord (shipSizes !! currentShip) otherShips
                                                
                                                -- there is space for the new ship
                                                if((length newShipCoords)/=0) then do
                                                    
                                                    -- store the newly positioned ship
                                                    put state {computerBoard = ((computerBoard state) {ships = otherShips ++ [newShipCoords]})}

                                                    -- move to the next one
                                                    initializeComputerBoard (currentShip+1)
                                                
                                                -- try again
                                                else initializeComputerBoard currentShip

                                            else initializeComputerBoard currentShip
                                        
                                         else liftIO$ putStrLn "Done Generating Computer Board!"

-- initialize player board
initializePlayerBoard :: Int -> StateT GameState IO()
initializePlayerBoard currentShip = do state <- get
                                       if(currentShip/=numShips) then do
                                        --liftIO$putStr ("(" ++ (shipNames !! currentShip) ++ " | len = " ++ show (shipSizes !! currentShip) ++ ") > ")
                                        -- (FINAL VERSION)
                                        --n <- liftIO$ getLine
                                        -- (TEST VERSION)
                                        let n = testInputs !! currentShip

                                        -- get the actual coordinates from this input string 
                                        let coords = parseInput n

                                        -- check if the coordinates have the right size
                                        if((getShipSize (coords !! 0) (coords !! 1))==(shipSizes !! currentShip)) then do

                                                -- check if the coordinates don't extend beyond the boards' limits or overlap with other ships
                                                if(checkCoords (coords !! 0) (coords !! 1) (ships (playerBoard state)) (shipSizes !! currentShip)) then do

                                                    -- compute the intermediate coordinates
                                                    let intermediateCoords = (getIntermediateCoords (coords !! 0) (coords !! 1))
                                                    
                                                    -- store the newly positioned ship
                                                    put state {playerBoard = ((playerBoard state) {ships = (ships (playerBoard state)) ++ [intermediateCoords]})}

                                                    -- move to the next one
                                                    initializePlayerBoard (currentShip+1)

                                                else do
                                                    liftIO$putStrLn ("Couldn't place ship (outside of the board)!")
                                                    initializePlayerBoard currentShip

                                        else do
                                            liftIO$putStrLn ("Couldn't match expected size " ++ show (shipSizes !! currentShip) ++ " with actual size " ++ show (getShipSize (coords !! 0) (coords !! 1)) ++ "!")
                                            initializePlayerBoard currentShip
                                       
                                       else liftIO$ putStrLn "Done Generating Player Board!"

-- jogar
-- put (computerBoard initialState) {board = checkPosition [[(1,2)],[(2,2)]]}
{--
pn = "(1,1);(1,5)"
nt1 = "(2,2);(5,2)"
nt2 = "(3,4);(3,7)"
ct1 = "(2,3);(2,5)"
ct2 = "(5,4);(7,4)"
ct3 = "(4,7);(6,7)"
s1 = "(4,0);(4,1)"
s2 = "(7,1);(7,2)"
s3 = "(2,0);(3,0)"
s4 = "(0,7);(1,7)"
--}


-- let's play!
initComputer = runStateT (initializeComputerBoard 0) initialState
initPlayer = runStateT (initializePlayerBoard 0) initialState