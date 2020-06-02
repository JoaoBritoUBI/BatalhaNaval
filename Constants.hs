module Constants where

boardSize :: Int
boardSize = 10   --O jogo deve ser implementado tendo em consideracao que este valor pode ser alterado.


-- Exemplos the navios. Podem ser utilizados para inicializar o tabuleiro sem ter de pedir input ao utilizador durante os testes do programa. 
-- Assume tamanho minimo do tabuleiro de 8.
-- Ter em atencao que os navios nao se podem sobrepor nem podem ter posicoes fora do tabuleiro. 
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
