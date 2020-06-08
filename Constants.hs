module Constants where

boardSize :: Int
boardSize = 10   --O jogo deve ser implementado tendo em consideracao que este valor pode ser alterado.

shipSizes :: [Int]
shipSizes = [5,4,4,3,3,3,2,2,2,2]

shipNames :: [[Char]]
shipNames = ["Porta-Aviões","Navio-Tanque","Navio-Tanque","Contratorpedeiro","Contratorpedeiro","Contratorpedeiro","Submarino","Submarino","Submarino","Submarino"]

numShips :: Int
numShips = length shipNames

-- Exemplos the navios. Podem ser utilizados para inicializar o tabuleiro sem ter de pedir input ao utilizador durante os testes do programa. 
-- Assume tamanho minimo do tabuleiro de 8.
-- Ter em atencao que os navios nao se podem sobrepor nem podem ter posicoes fora do tabuleiro. 
testInputs :: [String]
testInputs = ["(1,1);(1,5)","(2,2);(5,2)","(3,4);(3,7)","(2,3);(2,5)","(5,4);(7,4)","(4,7);(6,7)","(4,0);(4,1)","(7,1);(7,2)","(2,0);(3,0)","(0,7);(1,7)"]