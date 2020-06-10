module AuxFunctions where

import Data.List.Split

import Board

-- count how many occurrences of "positionState" are there in "positionStateList"
count :: PositionState -> [PositionState] -> Int
count positionState positionStateList = length (filter (==positionState) positionStateList)

-- checks if the coordinate "coord" has a ship in it 
hasShip :: Coord -> [[Coord]] -> Bool
hasShip coord ships = elem True (map (\x -> elem coord x) ships)

-- parses the input string and converts it into "Coord" format
parseInput :: String -> [Coord]
parseInput rawCoords = map (\[x,y] -> (x,y)) (map (\x -> map (\y -> read y :: Int) (splitOn "," x)) (splitOn ";" [x | x <- rawCoords, not (elem x "()")]))

stringToCoord :: String -> Coord
stringToCoord coordString = let aux = (map (\x -> read x :: Int) [[a] | a <- coordString, not (elem a "(),")]) in (aux !! 0, aux !! 1)

-- returns the size of a ship starting in "startCoord" and finishing in "endCoord"
getShipSize :: Coord -> Coord -> Int
getShipSize startCoord endCoord = (abs ((fst startCoord)-(fst endCoord))) + (abs ((snd startCoord)-(snd endCoord))) + 1

-- checks if we can place a ship within the given coordinates
checkCoords :: [Coord] -> [[Coord]] -> Bool
checkCoords shipCoords otherShips = (head shipCoords)>=(0,0) && (head (reverse shipCoords))>=(0,0) -- check if the ship extends outside of the boards' limits
                                    && (doesntOverlap shipCoords otherShips) -- check if this ship doesn't overlap with other ships

-- computes the missing coordinates between "startCoord" and "endCoord"
getIntermediateCoords :: Coord -> Coord -> [Coord]
getIntermediateCoords startCoord endCoord = if(startCoord<endCoord) then [(a,b) | a <- [(fst startCoord)..(fst endCoord)], b <- [(snd startCoord)..(snd endCoord)]]
                                            else reverse [(a,b) | a <- [(fst endCoord)..(fst startCoord)], b <- [(snd endCoord)..(snd startCoord)]]

-- checks if the ships "shipCoords" doesn't overlap with any other ship
doesntOverlap :: [Coord] -> [[Coord]] -> Bool
doesntOverlap shipCoords [] = True
doesntOverlap shipCoords (x:xs) = if((length (filter (\e -> elem e shipCoords) x))==0) then doesntOverlap shipCoords xs else False

-- attempts to position a ship with size "shipSize" starting from coordinates "startCoord"
-- if unsuccessful, returns an empty list; otherwise returns a list with the ships coordinates
getShipCoords :: Coord -> Int -> [[Coord]] -> [Coord]
getShipCoords startCoord shipSize otherShips = do 
                                                -- filter out impossible positions (i.e. outside the boards' limits) from the starting one
                                                let possibleEndCoords = filter (\(a,b) -> a>=0 && b>=0) [((fst startCoord)+(fst c),(snd startCoord)+(snd c)) | c <- [(-(shipSize-1),0),(0,(shipSize-1)),((shipSize-1),0),(0,-(shipSize-1))]]
                                                
                                                -- get the intermediate coordenates for every possible end coordinate
                                                let possibleShips = [(getIntermediateCoords startCoord e) | e <- possibleEndCoords]

                                                -- filter out the ships that would overlap with other ships
                                                let possibleShipsAux = [s | s <- possibleShips, (doesntOverlap s otherShips)]

                                                if((length possibleShipsAux)==0) then []
                                                else do
                                                    -- choose one of the available positions
                                                    possibleShipsAux !! 0