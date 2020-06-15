-- get ready for some PURE and IMPURE content
module AuxFunctions where

import Board
import Constants
import GameState
import Text.Read hiding (get)
import Data.List

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY, GENERAL PURPOSE, FUNCTIONS (PURE)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- safe implementation of the the "tail" function (if the list is empty, this version does not raise an exception)
safeTail :: [Coord] -> [Coord]
safeTail []   = []
safeTail list = tail list

-- transforms a tuple of lists into a list of lists (with a special case for the second part "b")
tupleToList :: ([Char],[Char]) -> [[Char]]
tupleToList (a,b) = [a,tail b]

-- parses the input string (supposedly, two coordinates separated by a ";") and converts it into "Coord" format
parseInput :: String -> [Coord]
parseInput rawCoords = do let aux = (splitOn ';' rawCoords) 
                          if((length aux)/=2) then [(-1,-1)]
                          else do 
                              let coords = (map (\x -> readMaybe x :: Maybe Coord) aux)
                              if(elem Nothing coords) then [(-1,-1)]
                              else (map fromJust coords)

-- counts how many occurrences of "positionState" are there in "positionStateList"
count :: PositionState -> [PositionState] -> Int
count positionState positionStateList = length (filter (==positionState) positionStateList)

-- checks if the coordinate "coord" is part of a ship
hasShip :: Coord -> [[Coord]] -> Bool
hasShip coord ships = elem True (map (\x -> elem coord x) ships)

-- returns the size of a ship starting in "startCoord" and finishing in "endCoord"
getShipSize :: Coord -> Coord -> Int
getShipSize startCoord endCoord = (abs ((fst startCoord)-(fst endCoord))) + (abs ((snd startCoord)-(snd endCoord))) + 1

-- checks if the given coordinate ("coord") fits inside the board
properCoord :: Coord -> Bool
properCoord coord = coord>=(0,0) && coord<(boardSize,boardSize)

-- checks if we can place a ship ("shipCoords") within the given coordinates ("otherShips")
checkCoords :: [Coord] -> [[Coord]] -> Bool
checkCoords shipCoords otherShips = (properCoord (head shipCoords)) && (properCoord (head (reverse shipCoords)))  -- check if the ship extends outside of the boards' limits
                                    && (doesntOverlap shipCoords otherShips) -- check if this ship doesn't overlap with other ships

-- computes the missing coordinates between "startCoord" and "endCoord"
getIntermediateCoords :: Coord -> Coord -> [Coord]
getIntermediateCoords startCoord endCoord = if(startCoord<endCoord) then [(a,b) | a <- [(fst startCoord)..(fst endCoord)], b <- [(snd startCoord)..(snd endCoord)]]
                                            else reverse [(a,b) | a <- [(fst endCoord)..(fst startCoord)], b <- [(snd endCoord)..(snd startCoord)]]

-- checks if the ships "shipCoords" doesn't overlap with any other ship
doesntOverlap :: [Coord] -> [[Coord]] -> Bool
doesntOverlap shipCoords []     = True
doesntOverlap shipCoords (x:xs) = if((length (filter (\e -> elem e shipCoords) x))==0) then doesntOverlap shipCoords xs else False

-- attempts to position a ship with size "shipSize" starting from coordinates "startCoord"
-- return values: [] - unable to place a ship with the given constraints; [Coord] - successfully placed a ship with the given constraints
getShipCoords :: Coord -> Int -> [[Coord]] -> [Coord]
getShipCoords startCoord shipSize otherShips = do 
                                                -- filter out impossible positions (i.e. outside the boards' limits) from the starting one
                                                let possibleEndCoords = filter (\(a,b) -> a>=0 && a<boardSize && b>=0 && b<boardSize) [((fst startCoord)+(fst c),(snd startCoord)+(snd c)) | c <- [(0,-(shipSize-1)),(-(shipSize-1),0),(0,(shipSize-1)),((shipSize-1),0)]]
                                                
                                                -- get the intermediate coordenates for every possible end coordinate
                                                let possibleShips = [(getIntermediateCoords startCoord e) | e <- possibleEndCoords]

                                                -- filter out the ships that would overlap with other ships
                                                let possibleShipsAux = [s | s <- possibleShips, (doesntOverlap s otherShips)]

                                                if((length possibleShipsAux)==0) then []
                                                else possibleShipsAux !! 0 -- choose one of the available positions

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AUXILIARY, GENERAL PURPOSE, FUNCTIONS (IMPURE)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- checks if the game's settings are OK
gameSettingsOK :: IO Bool
gameSettingsOK = if((length shipSizes)/=(length shipNames)) then do 
                    putStrLn ("\nFATAL ERROR: The ships' names and respective sizes do not match! Leaving the game...\n")
                    return False
                 else do
                    if((boardSize^2)<=(sum shipSizes)) then do 
                         putStrLn ("\nFATAL ERROR: The board has " ++ (show (boardSize^2)) ++ " available cells, but the ships would need " ++ (show (sum shipSizes)) ++ "! Leaving the game...\n")
                         return False
                    else return True -- the settings are OK

-- checks if the game has ended (i.e. when all the ships of one player have sunken)
hasGameEnded :: GameState -> Int
hasGameEnded state = do let playerWon = if((count Sunken (map (board (computerDefenseBoard state)) (concat (ships (computerDefenseBoard state)))))==(sum shipSizes)) then True else False
                        let computerWon = if((count Sunken (map (board (playerDefenseBoard state)) (concat (ships (playerDefenseBoard state)))))==(sum shipSizes)) then True else False
                        
                        -- retrieve the final decision
                        if(playerWon && computerWon) then 3 -- there has been a tie
                        else if(computerWon) then 2 -- only the computer has won
                             else if(playerWon) then 1 -- only the player has won
                                  else 0 -- the game must go on

-- retrieves the Coord value encapsulated by the Maybe type
fromJust :: Maybe Coord -> Coord
fromJust (Just a) = a
fromJust Nothing  = (-1,-1)

-- retrieves the Int value encapsulated by the Maybe type
fromJustInt :: Maybe Int -> Int
fromJustInt (Just a) = a
fromJustInt Nothing  = -1

-- split the input string ("string") on the given character "char"
splitOn :: Char -> String -> [String]
splitOn char string = do let index = elemIndex ';' string
                         if(index==Nothing) then []
                         else (tupleToList (splitAt (fromJustInt index) string))