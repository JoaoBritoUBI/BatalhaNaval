-- get ready for some PURE and IMPURE content
module GamePrints where

import Board
import Constants
import GameState

--------------------------------------------------------------------------
-- AUXILIARY FUNCTION RELATED TO THE GAME'S INTERFACE (PURE)
--------------------------------------------------------------------------
-- returns the number os spaces that a string needs
roundInfoNumSpaces :: Coord -> Int -> Int
roundInfoNumSpaces coord baseLength = (58 - baseLength - (length (show coord)) - 1)

-- returns the number os spaces that a string needs
gameOverNumSpaces :: Int -> Int
gameOverNumSpaces baseLength = (58 - baseLength - 3)

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY FUNCTIONS RELATED TO THE GAME'S INTERFACE (IMPURE)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- shows the game's instructions
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

-- shows the "Ships Placement" interface
showShipsPlacement :: IO ()
showShipsPlacement = do putStrLn "\no--------------------------------------------------------o"
                        putStrLn "|                     SHIP PLACEMENT                     |"
                        putStrLn "o--------------------------------------------------------o"
                        if(boardSize<=9) then putStrLn ("|                     Board Size = " ++ (show boardSize) ++ "                     |")
                        else putStrLn ("|                     Board Size = " ++ (show boardSize) ++ "                    |")
                        putStrLn "|                 Format = (start);(end)                 |"
                        putStrLn "o--------------------------------------------------------o"

-- shows the "Play" interface
showPlay :: IO ()
showPlay = do putStrLn "\no--------------------------------------------------------o"
              putStrLn "|                          PLAY                          |"
              putStrLn "o--------------------------------------------------------o\n"

-- shows the "RoundInfo" interface
showRoundInfo :: IO ()
showRoundInfo = do putStrLn "\no--------------------------------------------------------o"
                   putStrLn "|                       ROUND INFO                       |"
                   putStrLn "o--------------------------------------------------------o"

-- shows some information regarding the player's move
showPlayerRoundInfo :: Coord -> PositionState -> IO ()
showPlayerRoundInfo coord positionState = do if(positionState==Sunken) then putStrLn ("| (Player) Coordinate " ++ (show coord) ++ " lead to a SUNKEN SHIP!" ++ (replicate (roundInfoNumSpaces coord 45) ' ') ++ "|") 
                                             else if(positionState==Hit) then putStrLn ("| (Player) Coordinate " ++ (show coord) ++ " was a HIT!" ++ (replicate (roundInfoNumSpaces coord 33) ' ') ++ "|") 
                                                  else putStrLn ("| (Player) Coordinate " ++ (show coord) ++ " was a MISS!" ++ (replicate (roundInfoNumSpaces coord 34) ' ') ++ "|") 

-- shows some information regarding the computer's move
showComputerRoundInfo :: Coord -> PositionState -> IO ()
showComputerRoundInfo coord positionState = do if(positionState==Sunken) then putStrLn ("| (Computer) Coordinate " ++ (show coord) ++ " lead to a SUNKEN SHIP!" ++ (replicate (roundInfoNumSpaces coord 47) ' ') ++ "|") 
                                               else if(positionState==Hit) then putStrLn ("| (Computer) Coordinate " ++ (show coord) ++ " was a HIT!" ++ (replicate (roundInfoNumSpaces coord 35) ' ') ++ "|") 
                                                    else putStrLn ("| (Computer) Coordinate " ++ (show coord) ++ " was a MISS!" ++ (replicate (roundInfoNumSpaces coord 36) ' ') ++ "|") 

-- shows the "GameOver" interface
showGameOver :: String -> IO ()
showGameOver string = do putStrLn "o--------------------------------------------------------o"
                         putStrLn "|                       GAME OVER                        |"
                         putStrLn "o--------------------------------------------------------o"
                         putStrLn ("| " ++ string ++ (replicate (gameOverNumSpaces (length string)) ' ') ++ "|")
                         putStrLn "o--------------------------------------------------------o"

-- shows a single interface line
showSingleLine :: IO ()
showSingleLine = do putStrLn "o--------------------------------------------------------o\n"

-- shows both of the player's boards
showPlayerBoards :: GameState -> IO ()
showPlayerBoards state = do putStrLn ("   " ++ (replicate (((5+(4*(boardSize-1))) - 22)`div`2) ' ') ++ "Player's Offense Board")
                            print (board (playerOffenseBoard state))
                            putStrLn ("   " ++ (replicate (((5+(4*(boardSize-1))) - 22)`div`2) ' ') ++ "Player's Defense Board")
                            print (board (playerDefenseBoard state))