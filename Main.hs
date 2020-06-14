-- get ready for some IMPURE content
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

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- MAIN FUNCTION
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
play :: Bool -> StateT GameState IO()
play init = do state <- get
               if(init) then do -- initialize the boards
                    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                    -- get both the player and computer's ships and update the game's state
                    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                    -- show the "Ships Placement" interface
                    liftIO$showShipsPlacement

                    -- BEGIN FINAL VERSION
                    playerShips <- liftIO$(initializePlayerShips 0 [])
                    -- END FINAL VERSION

                    -- BEGIN TEST VERSION
                    --playerShips <- liftIO$(initializeComputerShips 0 [])
                    -- END TEST VERSION

                    if(playerShips==[[(-1,-1)]]) then liftIO$(putStrLn "\nLeaving the game...\n") -- the player wants to leave the game (pressed "q")
                    else do
                        liftIO$(putStrLn "\n(Player) Done placing the ships!")
                        
                        computerShips <- liftIO$(initializeComputerShips 0 [])

                        put state {
                            -- update the player's defense board function and its ships
                            playerDefenseBoard = ((playerDefenseBoard state) {board = (\x -> if(hasShip x playerShips) then Ship else ((board (playerDefenseBoard state)) x)), ships = playerShips}),
                            
                            -- update the computer's defense board function and its ships
                            computerDefenseBoard = ((computerDefenseBoard state) {board = (\x -> if(hasShip x computerShips) then Ship else ((board (computerDefenseBoard state)) x)), ships = computerShips})
                            }

                        -- show the "Play" interface
                        liftIO$showPlay
                        
                        play False

               else do -- play the game

                -- print both of the player's boards
                liftIO$(showPlayerBoards state)
                
                ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
                -- register the player and computer's moves
                ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
                state <- get
                
                -- BEGIN TEST VERSION
                {--
                -- get the computer's move
                let checkerboardBlack = filter (\x -> not (elem x (playerMoves state))) (masterCheckerboardBlack 0 [])
                let checkerboardWhite = filter (\x -> not (elem x (playerMoves state))) (masterCheckerboardWhite 0 [])
                let checkerboard = if(checkerboardBlack/=[]) then checkerboardBlack else checkerboardWhite

                playerMove <- liftIO$(getplayerMove (playerMoves state) checkerboard (playerNextMoves state))
                liftIO$(putStrLn ("\n(player) Attack position > " ++ (show playerMove)))
                --}
                -- END TEST VERSION
                
                -- BEGIN FINAL VERSION
                --{--
                -- get the player's move
                liftIO$putStr "(Player) Attack position > "
                playerMove <- liftIO$(getPlayerMove (playerMoves state))
                --}
                -- END FINAL VERSION
                
                if(playerMove==(-1,-1)) then liftIO$(putStrLn "\nLeaving the game...\n") -- the player wants to leave the game (pressed "q")
                else do
                    -- get the computer's move
                    let checkerboardBlack = filter (\x -> not (elem x (computerMoves state))) (masterCheckerboardBlack 0 [])
                    let checkerboardWhite = filter (\x -> not (elem x (computerMoves state))) (masterCheckerboardWhite 0 [])
                    let checkerboard = if(checkerboardBlack/=[]) then checkerboardBlack else checkerboardWhite

                    computerMove <- liftIO$(getComputerMove (computerMoves state) checkerboard (computerNextMoves state))
                    liftIO$(putStrLn ("\n(Computer) Attack position > " ++ (show computerMove)))

                    -- save the new information
                    put state {
                        -- update the player's part
                        playerMoves = (playerMoves state) ++ [playerMove],

                        -- update the computer's part
                        computerMoves = (computerMoves state) ++ [computerMove],

                        -- update the computer's priority queue
                        computerNextMoves = (removeCoordFromQueue computerMove (computerNextMoves state))

                        -- BEGIN TEST VERSION 
                        -- update the player's priority queue
                        --playerNextMoves = (drop 1 (playerNextMoves state))
                        -- END TEST VERSION 
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

                                    -- update the player's priority queue
                                    -- BEGIN TEST VERSION
                                    --playerNextMoves = ((playerNextMoves state) ++ (getUnvisitedNeighbours playerMove (playerMoves state) (playerNextMoves state)))
                                    -- END TEST VERSION
                                }

                    else -- case where the player didn't hit anything
                        do
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
                                    playerDefenseBoard = (playerDefenseBoard state) {board = (\x -> if(x==computerMove) then Hit else ((board (playerDefenseBoard state)) x))},

                                    -- update the computer's priority queue
                                    computerNextMoves = ((computerNextMoves state) ++ (getUnvisitedNeighbours computerMove (computerMoves state) (computerNextMoves state)))
                                }

                    else -- case where the computer didn't hit anything
                        do
                            liftIO$(showComputerRoundInfo computerMove Miss)
                            put state {
                                -- update the player's offense board
                                computerOffenseBoard = (computerOffenseBoard state) {board = (\x -> if(x==computerMove) then Miss else ((board (computerOffenseBoard state)) x))},
                                
                                -- update the computer's defense board
                                playerDefenseBoard = (playerDefenseBoard state) {board = (\x -> if(x==computerMove) then Miss else ((board (playerDefenseBoard state)) x))}
                            }

                    liftIO$showSingleLine
                    state <- get
                    liftIO$print (computerNextMoves state)

                    -----------------------------------------------------------------------------------------
                    -- detect when the game ends and show the "Game Over" interface 
                    -----------------------------------------------------------------------------------------
                    state <- get
                    let endGame = hasGameEnded state
                    if(endGame==0) then play False
                    else do
                        if(endGame==1) then liftIO$showGameOver "The player has won the game!"

                        else 
                            if(endGame==2) then liftIO$showGameOver "The computer has won the game!"
                            else liftIO$showGameOver "It's a tie! Both the player and the computer have won!"

----------------------------------------------------------
-- MAIN CONTROLS
----------------------------------------------------------
main :: IO ((), GameState)
main = do settingsOK <- liftIO$gameSettingsOK

          if(settingsOK) then do -- the settings are OK
            wantsToPlay <- liftIO$showInstructions

            if(wantsToPlay) then -- the user wants to play
                runStateT (play True) initialState
            
            else return ((),initialState)

          else return ((),initialState)