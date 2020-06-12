-- contains IMPURE content
module Main where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import GameState
import Board
import Constants
import AuxFunctions
import GamePrints
import GameMoves

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- MAIN FUNCTION
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
play :: Bool -> Coord -> StateT GameState IO()
play init enter = do state <- get
                     if(init) then do -- initialize the boards

                        if(enter==((-1,-1))) then liftIO$putStrLn "\nLeaving the game...\n" -- the player wants to leave the game (pressed "q")
                        else do
                            ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                            -- get both the player's and computer's ships and update the state
                            ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                            -- show the "Ships Placement" interface
                            liftIO$showShipsPlacement

                            playerShips <- liftIO$(initializePlayerShips 0 [])
                            if(playerShips==[[(-1,-1)]]) then liftIO$putStrLn "\nLeaving the game...\n" -- the player wants to leave the game (pressed "q")
                            else do
                                liftIO$putStrLn "\n(Player) Done placing the ships!"
                                
                                computerShips <- liftIO$(initializeComputerShips 0 [])

                                put state {
                                    -- update the player's defense board function and its ships
                                    playerDefenseBoard = ((playerDefenseBoard state) {board = (\x -> if(hasShip x playerShips) then Ship else ((board (playerDefenseBoard state)) x)), ships = playerShips}),
                                    
                                    -- update the computer's defense board function and its ships
                                    computerDefenseBoard = ((computerDefenseBoard state) {board = (\x -> if(hasShip x computerShips) then Ship else ((board (computerDefenseBoard state)) x)), ships = computerShips})
                                    }

                                -- show the "Play" interface
                                liftIO$showPlay
                                
                                play False enter

                     else do -- play the game

                        -- print both of the player's boards
                        liftIO$(showPlayerBoards state)

                        ------------------------------------------------------------------------------
                        -- register the player's and computer's moves
                        ------------------------------------------------------------------------------
                        -- get the player's move
                        liftIO$putStr "(Player) Attack position > "
                        
                        -- TEST VERSION
                        --playerMove <- liftIO$getComputerMove (playerMoves state)
                        --liftIO$putStrLn ((show playerMove) ++ "\n")
                        
                        -- FINAL VERSION
                        playerMove <- liftIO$getPlayerMove (playerMoves state)
                        
                        if(playerMove==(-1,-1)) then liftIO$putStrLn "\nLeaving the game...\n"
                        else do
                            -- get the computer's move
                            computerMove <- liftIO$getComputerMove (computerMoves state)
                            liftIO$putStrLn ("\n(Computer) Attack position > " ++ (show computerMove))

                            -- save the new information
                            put state {
                                -- update the player's part
                                playerMoves = (playerMoves state) ++ [playerMove],

                                -- update the computer's part
                                computerMoves = (computerMoves state) ++ [computerMove]
                            }
                            
                            -- show the "Round Info" interface
                            liftIO$showRoundInfo

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
                                        liftIO$(showPlayerRoundInfo playerMove Sunken)
                                        put state {

                                            -- update the player's offense board
                                            playerOffenseBoard = (playerOffenseBoard state) {board = (\x -> if(elem x ship) then Sunken else ((board (playerOffenseBoard state)) x))},
                                            
                                            -- update the computer's defense board
                                            computerDefenseBoard = (computerDefenseBoard state) {board = (\x -> if(elem x ship) then Sunken else ((board (computerDefenseBoard state)) x))}
                                        }

                                    else -- case where a part of a ship was bombed
                                        do
                                        liftIO$(showPlayerRoundInfo playerMove Hit)
                                        put state {
                                            -- update the player's offense board
                                            playerOffenseBoard = (playerOffenseBoard state) {board = (\x -> if(x==playerMove) then Hit else ((board (playerOffenseBoard state)) x))},
                                            
                                            -- update the computer's defense board
                                            computerDefenseBoard = (computerDefenseBoard state) {board = (\x -> if(x==playerMove) then Hit else ((board (computerDefenseBoard state)) x))}
                                            }

                            else do
                                liftIO$(showPlayerRoundInfo playerMove Miss)
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
                                        liftIO$(showComputerRoundInfo computerMove Sunken)
                                        put state {

                                            -- update the computer's offense board
                                            computerOffenseBoard = (computerOffenseBoard state) {board = (\x -> if(elem x ship) then Sunken else ((board (computerOffenseBoard state)) x))},
                                            
                                            -- update the player's defense board
                                            playerDefenseBoard = (playerDefenseBoard state) {board = (\x -> if(elem x ship) then Sunken else ((board (playerDefenseBoard state)) x))}
                                        }

                                    else -- case where a part of a ship was bombed
                                        do
                                        liftIO$(showComputerRoundInfo computerMove Hit)
                                        put state {
                                            -- update the computer's offense board
                                            computerOffenseBoard = (computerOffenseBoard state) {board = (\x -> if(x==computerMove) then Hit else ((board (computerOffenseBoard state)) x))},
                                            
                                            -- update the player's defense board
                                            playerDefenseBoard = (playerDefenseBoard state) {board = (\x -> if(x==computerMove) then Hit else ((board (playerDefenseBoard state)) x))}
                                            }
                            else do
                                liftIO$(showComputerRoundInfo computerMove Miss)
                                put state {
                                    -- update the player's offense board
                                    computerOffenseBoard = (computerOffenseBoard state) {board = (\x -> if(x==computerMove) then Miss else ((board (computerOffenseBoard state)) x))},
                                    
                                    -- update the computer's defense board
                                    playerDefenseBoard = (playerDefenseBoard state) {board = (\x -> if(x==computerMove) then Miss else ((board (playerDefenseBoard state)) x))}
                                    }

                            liftIO$showSingleLine

                            -----------------------------------------------------------------------------------------
                            -- detect when the game ends
                            -----------------------------------------------------------------------------------------
                            state <- get
                            let endGame = hasGameEnded state
                            if(endGame==0) then play False enter
                            else do
                                -- show the "Game Over" interface
                                liftIO$showGameOver

                                if(endGame==1) then do
                                    liftIO$putStrLn "\nThe player has won the game!\n"

                                else 
                                    if(endGame==2) then liftIO$putStrLn "\nThe computer has won the game!\n"
                                    else liftIO$putStrLn "\nIt's a tie! Both the player and the computer have won!\n"

--------------------------------------------------
-- MAIN CONTROLS
--------------------------------------------------
main :: IO ((), GameState)
main = do enter <- liftIO$showInstructions
          runStateT (play True enter) initialState