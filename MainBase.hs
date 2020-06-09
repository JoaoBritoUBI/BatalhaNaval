import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Exception
import Data.Char
import System.Random

import GameState
import Board
import Constants
import AuxFunctions

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY FUNCTIONS (THAT ARE NOT PURE)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- generates random coordinates
getRandomCoordinates :: IO Coord
getRandomCoordinates = do
    x <- randomIO
    y <- randomIO
    return (x `mod` boardSize, y `mod` boardSize)

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
                                            let newShipCoords = (getShipCoords startCoord (shipSizes !! currentShip) otherShips)
                                            
                                            -- there is space for the new ship
                                            if((length newShipCoords)/=0) then do
                                                
                                                -- store the newly positioned ship
                                                put state {computerBoard = ((computerBoard state) {ships = otherShips ++ [newShipCoords]})}

                                                -- move to the next one
                                                initializeComputerBoard (currentShip+1)
                                            
                                            -- try again
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

                                            -- compute the intermediate coordinates
                                            let intermediateCoords = (getIntermediateCoords (coords !! 0) (coords !! 1))

                                            -- check if the coordinates don't extend beyond the boards' limits or overlap with other ships
                                            if(checkCoords intermediateCoords (ships (playerBoard state))) then do
                                                
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

-- play the game
-- put (computerBoard initialState) {board = checkPosition [[(1,2)],[(2,2)]]}

-- let's play!
initComputer = runStateT (initializeComputerBoard 0) initialState
initPlayer = runStateT (initializePlayerBoard 0) initialState