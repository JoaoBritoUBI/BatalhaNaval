import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Char
import System.Random
import Text.Read hiding (get)

import GameState
import Board
import Constants
import AuxFunctions

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY FUNCTIONS (THAT ARE NOT PURE)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- checks if the game has ended (i.e. when all the ships of one player have sunken)
-- return values: 0 - the game has not ended; 1 - the player has won; 2 - there has been a tie; 3 - the computer has won
hasGameEnded :: GameState -> IO Int
hasGameEnded state = do let computerShips = concat (ships (computerDefenseBoard state))
                        if((count Sunken (map (board (computerDefenseBoard state)) computerShips))==(sum shipSizes)) then do -- the player has won
                            let playerShips = concat (ships (playerDefenseBoard state))
                            if((count Sunken (map (board (playerDefenseBoard state)) playerShips))==(sum shipSizes)) then do -- the computer has also won, it's a tie
                                return 2
                            else return 1
                        else do
                            let playerShips = concat (ships (playerDefenseBoard state))
                            if((count Sunken (map (board (playerDefenseBoard state)) playerShips))==(sum shipSizes)) then do -- the computer has won
                                    return 3
                            else return 0

-- parses the input string (supposedly, two coordinates separated by a ";") and converts it into "Coord" format
parseInput :: String -> IO [Coord]
parseInput rawCoords = do let aux = (splitOn ";" rawCoords) 
                          if((length aux)/=2) then return [(-1,-1)]
                          else do 
                              let coords = (map (\x -> readMaybe x :: Maybe Coord) aux)
                              if(elem Nothing coords) then return [(-1,-1)]
                              else return (map fromJust coords)

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
getPlayerMove alreadyChosen = do n <- getLine
                                 if(n=="q") then return (-1,-1) -- leave the game
                                 else do
                                    let aux = readMaybe n :: Maybe Coord

                                    -- the coordinate in in the wrong format
                                    if(aux==Nothing) then do
                                        liftIO$putStr "(Player) Wrong format! Try again > "
                                        getPlayerMove alreadyChosen
                                    
                                    -- the coordinate was already chosen
                                    else do
                                        if(elem (fromJust aux) alreadyChosen) then do
                                            putStr "(Player) Already chosen! Try again > "
                                            getPlayerMove alreadyChosen
                                        
                                        -- properly chosen coordinate
                                        else return (fromJust aux)

----------------------------------------------------------------------------------------------------------------------------------------------------------------
-- SHOW THE GAME'S INSTRUCTIONS AT THE BEGINNING
----------------------------------------------------------------------------------------------------------------------------------------------------------------
showInstructions :: IO Coord
showInstructions = do putStrLn "\no---------------------------------------------------------------------------------------------------------------------------o"
                      putStrLn "|                                                   BATTLESHIP IN HASKELL                                                   |"
                      putStrLn "o---------------------------------------------------------------------------------------------------------------------------o"
                      putStrLn "|                                                           Rules                                                           |"
                      putStrLn "| 1. Ship Placement                                                                                                         |"
                      putStrLn "o------------------                                                                                                         |"
                      putStrLn "| * The ships' coordinates are connected in straight lines (i.e. no diagonals allowed)                                      |"
                      putStrLn "| * The ships can not extend beyond the board or overlap with each other                                                    |"
                      putStrLn "| * When defining a ship's coordinates, it should be done in the following format: \"(Int,Int);(Int,Int)\"                    |"
                      putStrLn "|                                                                                                                           |"
                      putStrLn "| 2. General Rules                                                                                                          |"
                      putStrLn "o-----------------                                                                                                          |"
                      putStrLn "| * The player plays first, followed by the computer                                                                        |"
                      putStrLn "| * The player has access to 2 boards:                                                                                      |"
                      putStrLn "|   º Offense Board (top): where the player marks the chosen attacks, without knowing the positions of the opponent's ships |"
                      putStrLn "|   º Defense Board (bottom): where the player's ships are, as well as, markings of the opponent's attacks                  |"
                      putStrLn "| * The player can not repeat attack positions (they are \"one-time-only\")                                                   |"
                      putStrLn "| * There will be appropriate messages to the game's outcomes (HIT, MISS and SUNKEN events)                                 |"
                      putStrLn "| * Whenever asked to give input, the player can press the letter \"q\" to leave the game immediately                         |"
                      putStrLn "| * The game ends when one(both) player(s) has(have) no ships left                                                          |"
                      putStrLn "| * Upon finishing, the program will show where both players' ships were and how their defense boards ended up              |"
                      putStrLn "|                                                                                                                           |"
                      putStrLn "| GOOD LUCK! LET'S PLAY!                                                                                                    |"
                      putStrLn "o---------------------------------------------------------------------------------------------------------------------------o"
                      
                      putStr "\n(Player) Press any key to start or \"q\" to leave the game > "
                      
                      enter <- getLine
                      if(enter=="q") then return (-1,-1)
                      else return (0,0)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- INITIALIZE THE PLAYER'S SHIPS
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
initializePlayerShips :: Int -> [[Coord]] -> IO [[Coord]]
initializePlayerShips currentShip finalList = do 
                                                if(currentShip/=numShips) then do
                                                    -- TEST VERSION
                                                    let n = testInputs !! currentShip
                                                    
                                                    -- FINAL VERSION
                                                    --liftIO$putStr ("\n" ++ (shipNames !! currentShip) ++ " | len = " ++ show (shipSizes !! currentShip) ++ " > ")
                                                    -- read and check the input
                                                    --n <- liftIO$ getLine

                                                    if(n=="q") then return [[(-1,-1)]] -- leave the game
                                                    else do
                                                        -- get the actual coordinates from the input string 
                                                        coords <- parseInput n

                                                        -- wrong format
                                                        if(coords==[(-1,-1)]) then do
                                                            liftIO$putStrLn "(Player) Wrong format! Try again!"
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
                                                                    liftIO$putStrLn "(Player) The ship extends beyond the board! Try again!"
                                                                    initializePlayerShips currentShip finalList

                                                            else do
                                                                liftIO$putStrLn "(Player) The ship doesn't have the correct size! Try again!"
                                                                initializePlayerShips currentShip finalList
                                                
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

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- PLAY THE GAME
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
play :: Bool -> Coord -> StateT GameState IO()
play init enter = do state <- get
                     if(init) then do -- initialize the boards

                        if(enter==((-1,-1))) then liftIO$putStrLn "\nLeaving the game...\n"
                        else do
                            ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                            -- get both the player's and computer's ships and update the state
                            ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                            liftIO$putStrLn "\no---------------------------------------------------------------------------------------------------------------------------o"
                            liftIO$putStrLn "|                                                      SHIP PLACEMENT                                                       |"
                            liftIO$putStrLn "o---------------------------------------------------------------------------------------------------------------------------o"
                            if(boardSize<=9) then liftIO$putStrLn ("|                                                      Board Size = " ++ (show boardSize) ++ "                                                       |")
                            else liftIO$putStrLn ("|                                                      Board Size = " ++ (show boardSize) ++ "                                                      |")
                            liftIO$putStrLn "|                                                  Format = (start);(end)                                                   |"
                            liftIO$putStrLn "o---------------------------------------------------------------------------------------------------------------------------o"

                            playerShips <- liftIO$(initializePlayerShips 0 [])
                            if(playerShips==[[(-1,-1)]]) then liftIO$putStrLn "\nLeaving the game...\n"
                            else do
                                liftIO$putStrLn "\n(Player) Done placing the ships!"
                                
                                computerShips <- liftIO$(initializeComputerShips 0 [])

                                put state {
                                    -- update the player's defense board function and its ships
                                    playerDefenseBoard = ((playerDefenseBoard state) {board = (\x -> if(hasShip x playerShips) then Ship else ((board (playerDefenseBoard state)) x)), ships = playerShips}),
                                    
                                    -- update the computer's defense board function and its ships
                                    computerDefenseBoard = ((computerDefenseBoard state) {board = (\x -> if(hasShip x computerShips) then Ship else ((board (computerDefenseBoard state)) x)), ships = computerShips})
                                    }

                                liftIO$putStrLn "\no---------------------------------------------------------------------------------------------------------------------------o"
                                liftIO$putStrLn "|                                                           PLAY                                                            |"
                                liftIO$putStrLn "o---------------------------------------------------------------------------------------------------------------------------o\n"

                                play False enter

                     else do -- play the game

                        -----------------------------------------------
                        -- print both of the player's boards
                        -----------------------------------------------
                        liftIO$putStrLn "|  Player's Offense Board  |"
                        liftIO$print (board (playerOffenseBoard state))
                        liftIO$putStrLn "|  Player's Defense Board  |"
                        liftIO$print (board (playerDefenseBoard state))

                        ----------------------------------------------------------------------------
                        -- register the player's and computer's moves
                        ----------------------------------------------------------------------------
                        -- get the player's move
                        liftIO$putStr "(Player) Attack position > "
                        
                        -- TEST VERSION
                        playerMove <- liftIO$getComputerMove (playerMoves state)
                        liftIO$putStrLn ((show playerMove) ++ "\n")
                        
                        -- FINAL VERSION
                        --playerMove <- liftIO$getPlayerMove (playerMoves state)
                        
                        if(playerMove==(-1,-1)) then liftIO$putStrLn "\nLeaving the game...\n"
                        else do
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

                            -----------------------------------------------------------------------------------------------------------------------------------------------------
                            -- detect when the game ends
                            -----------------------------------------------------------------------------------------------------------------------------------------------------
                            state <- get
                            endGame <- liftIO$(hasGameEnded state)
                            if(endGame==0) then play False enter
                            else do
                                liftIO$putStrLn "\no---------------------------------------------------------------------------------------------------------------------------o"
                                liftIO$putStrLn "|                                                        GAME OVER                                                          |"
                                liftIO$putStrLn "o---------------------------------------------------------------------------------------------------------------------------o"

                                if(endGame==1) then do -- the player has won
                                    liftIO$putStrLn "\nThe player has won the game!\n"

                                else 
                                    if(endGame==2) then liftIO$putStrLn "\nBoth the player and the computer have won!\n"
                                    else liftIO$putStrLn "\nThe computer has won the game!\n"

--------------------------------------------------
-- MAIN CONTROLS
--------------------------------------------------
main :: IO ((), GameState)
main = do enter <- liftIO$showInstructions
          runStateT (play True enter) initialState