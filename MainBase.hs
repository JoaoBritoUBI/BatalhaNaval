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

-------------------------------------------------------------------------------------------------
-- AUXILIARY FUNCTIONS (THAT ARE NOT PURE)
-------------------------------------------------------------------------------------------------
-- generates random coordinates
getRandomCoordinates :: IO Coord
getRandomCoordinates = do
    x <- randomIO
    y <- randomIO
    return (x `mod` boardSize, y `mod` boardSize)

-- randomly choose a coordinate for the computer's attack
getComputerMove :: [Coord] -> IO Coord
getComputerMove alreadyChosen = do move <- liftIO$getRandomCoordinates
                                   if(elem move alreadyChosen) then getComputerMove alreadyChosen
                                   else return move

-- get a coordinate for the player's attack
getPlayerMove :: [Coord] -> IO Coord
getPlayerMove alreadyChosen = do aux <- getLine
                                 if(elem (stringToCoord aux) alreadyChosen) then 
                                     do liftIO$putStr "(Player) Already chosen! Try again > "
                                        getPlayerMove alreadyChosen
                                 else return (stringToCoord aux)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- INITIALIZE THE PLAYER'S SHIPS
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
initializePlayerShips :: Int -> [[Coord]] -> IO [[Coord]]
initializePlayerShips currentShip finalList = do 
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
                                                        let newShipCoords = (getIntermediateCoords (coords !! 0) (coords !! 1))

                                                        -- check if the coordinates don't extend beyond the boards' limits or overlap with other ships
                                                        if(checkCoords newShipCoords finalList) then do

                                                            -- move to the next one
                                                            initializePlayerShips (currentShip+1) (finalList ++ [newShipCoords])

                                                        else initializePlayerShips currentShip finalList

                                                    else initializePlayerShips currentShip finalList
                                                
                                                else return finalList

---------------------------------------------------------------------------------------------------------------------------------------
-- INITIALIZE THE COMPUTER'S SHIPS
---------------------------------------------------------------------------------------------------------------------------------------
initializeComputerShips :: Int -> [[Coord]] -> IO [[Coord]]
initializeComputerShips currentShip finalList = do 
                                                if(currentShip/=numShips) then do

                                                    startCoord <- liftIO$getRandomCoordinates
                                                
                                                    -- try to position a ship with size "currentShip" starting from coordinates "coord"
                                                    let newShipCoords = (getShipCoords startCoord (shipSizes !! currentShip) finalList)
                                                    
                                                    -- there is space for the new ship
                                                    if((length newShipCoords)/=0) then do

                                                        -- move to the next one
                                                        initializeComputerShips (currentShip+1) (finalList ++ [newShipCoords])
                                                    
                                                    -- try again
                                                    else initializeComputerShips currentShip finalList
                                                
                                                else return finalList

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- PLAY THE GAME
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
play :: IO [[Coord]] -> IO [[Coord]] -> Bool -> StateT GameState IO()
play pShipsInit cShipsInit init = do state <- get
                                     if(init) then do -- initialize the boards
                                        
                                        --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                        -- get both the player's and computer's ships and update the state
                                        --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                        playerShips <- liftIO$pShipsInit
                                        computerShips <- liftIO$cShipsInit

                                        put state {
                                            -- update the player's defense board function and its ships
                                            playerDefenseBoard = ((playerDefenseBoard state) {board = (\x -> if(hasShip x playerShips) then Ship else ((board (playerDefenseBoard state)) x)), ships = playerShips}),
                                            
                                            -- update the computer's defense board function and its ships
                                            computerDefenseBoard = ((computerDefenseBoard state) {board = (\x -> if(hasShip x computerShips) then Ship else ((board (computerDefenseBoard state)) x)), ships = computerShips})
                                            }

                                        play pShipsInit cShipsInit False

                                     else do -- play the game

                                        -----------------------------------------------
                                        -- print both of the player's boards
                                        -----------------------------------------------
                                        liftIO$putStrLn "|  Player's Offense Board  |"
                                        liftIO$print (board (playerOffenseBoard state))
                                        liftIO$putStrLn "|  Player's Defense Board  |"
                                        liftIO$print (board (playerDefenseBoard state))

                                        ------------------------------------------------------------------------
                                        -- register the player's and computer's moves
                                        ------------------------------------------------------------------------
                                        -- get the player's move
                                        liftIO$putStr "(Player) Attack position > "
                                        playerMove <- liftIO$getPlayerMove (playerMoves state)

                                        -- get the computer's move
                                        computerMove <- liftIO$getComputerMove (computerMoves state)
                                        liftIO$putStrLn ("(Computer) Attack position > " ++ (show computerMove))

                                        -- save the new information
                                        put state {
                                            -- update the player's part
                                            playerMoves = (playerMoves state) ++ [playerMove],

                                            -- update the computer's part
                                            computerMoves = (computerMoves state) ++ [computerMove]
                                        }
                                        
                                        liftIO$putStr "\n"

                                        ---------------------------------------------------------------------------------------------------------------------------------------------------------------
                                        -- deal with the player's move
                                        ---------------------------------------------------------------------------------------------------------------------------------------------------------------
                                        state <- get
                                        if(hasShip playerMove (ships (computerDefenseBoard state))) then 
                                            do
                                                let ship = (filter (\x -> elem playerMove x) (ships (computerDefenseBoard state))) !! 0

                                                -- case where the whole ship is bombed (i.e. should be updated to the state "Sunken")
                                                if((count Hit (map (board (computerDefenseBoard state)) ship))==((length ship)-1)) then
                                                    do
                                                    liftIO$putStrLn ("(Player) Coordinate " ++ (show playerMove) ++ " lead do a SUNKEN SHIP!")
                                                    put state {

                                                        -- update the player's offense board
                                                        playerOffenseBoard = (playerOffenseBoard state) {board = (\x -> if(elem x ship) then Sunken else ((board (playerOffenseBoard state)) x))},
                                                        
                                                        -- update the computer's defense board
                                                        computerDefenseBoard = (computerDefenseBoard state) {board = (\x -> if(elem x ship) then Sunken else ((board (computerDefenseBoard state)) x))}
                                                    }

                                                else -- case where a part of a ship was bombed
                                                    do
                                                    liftIO$putStrLn ("(Player) Coordinate " ++ (show playerMove) ++ " was a HIT!")
                                                    put state {
                                                        -- update the player's offense board
                                                        playerOffenseBoard = (playerOffenseBoard state) {board = (\x -> if(x==playerMove) then Hit else ((board (playerOffenseBoard state)) x))},
                                                        
                                                        -- update the computer's defense board
                                                        computerDefenseBoard = (computerDefenseBoard state) {board = (\x -> if(x==playerMove) then Hit else ((board (computerDefenseBoard state)) x))}
                                                        }

                                        else do
                                            liftIO$putStrLn ("(Player) Coordinate " ++ (show playerMove) ++ " was a MISS!")
                                            put state {
                                                -- update the player's offense board
                                                playerOffenseBoard = (playerOffenseBoard state) {board = (\x -> if(x==playerMove) then Miss else ((board (playerOffenseBoard state)) x))},
                                                
                                                -- update the computer's defense board
                                                computerDefenseBoard = (computerDefenseBoard state) {board = (\x -> if(x==playerMove) then Miss else ((board (computerDefenseBoard state)) x))}
                                                }

                                        -----------------------------------------------------------------------------------------------------------------------------------------------------------------
                                        -- deal with the computer's move
                                        -----------------------------------------------------------------------------------------------------------------------------------------------------------------
                                        state <- get
                                        if(hasShip computerMove (ships (playerDefenseBoard state))) then 
                                            do
                                                let ship = (filter (\x -> elem computerMove x) (ships (playerDefenseBoard state))) !! 0

                                                -- case where the whole ship is bombed (i.e. should be updated to the state "Sunken")
                                                if((count Hit (map (board (playerDefenseBoard state)) ship))==((length ship)-1)) then
                                                    do
                                                    liftIO$putStrLn ("(Computer) Coordinate " ++ (show computerMove) ++ " lead do a SUNKEN SHIP!")
                                                    put state {

                                                        -- update the computer's offense board
                                                        computerOffenseBoard = (computerOffenseBoard state) {board = (\x -> if(elem x ship) then Sunken else ((board (computerOffenseBoard state)) x))},
                                                        
                                                        -- update the player's defense board
                                                        playerDefenseBoard = (playerDefenseBoard state) {board = (\x -> if(elem x ship) then Sunken else ((board (playerDefenseBoard state)) x))}
                                                    }

                                                else -- case where a part of a ship was bombed
                                                    do
                                                    liftIO$putStrLn ("(Computer) Coordinate " ++ (show computerMove) ++ " was a HIT!")
                                                    put state {
                                                        -- update the computer's offense board
                                                        computerOffenseBoard = (computerOffenseBoard state) {board = (\x -> if(x==computerMove) then Hit else ((board (computerOffenseBoard state)) x))},
                                                        
                                                        -- update the player's defense board
                                                        playerDefenseBoard = (playerDefenseBoard state) {board = (\x -> if(x==computerMove) then Hit else ((board (playerDefenseBoard state)) x))}
                                                        }
                                        else do
                                            liftIO$putStrLn ("(Computer) Coordinate " ++ (show computerMove) ++ " was a MISS!")
                                            put state {
                                                -- update the player's offense board
                                                computerOffenseBoard = (computerOffenseBoard state) {board = (\x -> if(x==computerMove) then Miss else ((board (computerOffenseBoard state)) x))},
                                                
                                                -- update the computer's defense board
                                                playerDefenseBoard = (playerDefenseBoard state) {board = (\x -> if(x==computerMove) then Miss else ((board (playerDefenseBoard state)) x))}
                                                }

                                        liftIO$putStr "\n"
                                        play pShipsInit cShipsInit False

---------------------------------------------------------------
-- MAIN CONTROLS
---------------------------------------------------------------
pShipsInit = initializePlayerShips 0 []
cShipsInit = initializeComputerShips 0 []

main = runStateT (play pShipsInit cShipsInit True) initialState