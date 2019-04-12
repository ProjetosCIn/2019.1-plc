-- Questão 1 -  AP 3
-- [IF686EC] - 2019.1
-- autor: jrsf e tasm2

-- Criando a tabela

type Grupo = (Char, Time, Time, Time, Time)
data Time = Egito | Russia | Arabia | Uruguai |
            Ira | Marrocos | Portugal | Espanha
            deriving (Eq, Show)
type Tabela = [(Time, Int, Int, Int)]
type Jogo = (Time, Int, Int, Time) 

grupoA :: Grupo
grupoA = ('A', Egito, Russia, Arabia, Uruguai)
jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 1, 1, Uruguai), 
          (Egito, 1, 0, Arabia),(Russia, 2, 2, Uruguai), 
          (Russia, 0, 1, Arabia), (Egito, 2, 2, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]

getListaFromGrupo :: Grupo -> [Time]
getListaFromGrupo (x, t1, t2, t3, t4) = [t1, t2, t3, t4]

tratarJogos :: Grupo -> [Jogo] -> [Jogo]
tratarJogos group [] = []
tratarJogos group ((a, b, c, d):xs)
  | elem a (getListaFromGrupo group) && elem d (getListaFromGrupo group) = (a , b, c, d) : (tratarJogos group xs)
  | otherwise = (tratarJogos group xs)

updateWin :: Jogo -> Tabela -> Tabela
updateWin (a,b,c,d) tabela = (updateLoser (c-b) c d (updateWinner (b-c) b a tabela))

updateDraw :: Jogo -> Tabela -> Tabela
updateDraw (a,b,c,d) tabela = (updateDrawer b a (updateDrawer c d tabela))

updateWinner :: Int -> Int -> Time -> Tabela -> Tabela
updateWinner x y z [] = []
updateWinner saldo gol time ((timeTabela, pontos, sg, gf):xs)
  | time == timeTabela = (timeTabela, pontos + 3, sg + saldo, gf + gol) : xs
  | otherwise = (timeTabela, pontos, sg, gf):(updateWinner saldo gol time xs)

updateLoser :: Int -> Int -> Time -> Tabela -> Tabela
updateLoser x y z [] = []
updateLoser saldo gol time ((timeTabela, pontos, sg, gf):xs)
  | time == timeTabela = (timeTabela, pontos, sg + saldo, gf + gol) : xs
  | otherwise = (timeTabela, pontos, sg, gf):(updateLoser saldo gol time xs)

updateDrawer :: Int -> Time -> Tabela -> Tabela
updateDrawer x z [] = []
updateDrawer gol time ((timeTabela, pontos, sg, gf):xs)
  | time == timeTabela = (timeTabela, pontos + 1, sg, gf + gol) : xs
  | otherwise = (timeTabela, pontos, sg, gf):(updateDrawer gol time xs)

createTabelaFromGrupo :: Grupo -> Tabela
createTabelaFromGrupo (a,b,c,d,e) = [(b, 0,0,0), (c, 0,0,0), (d, 0,0,0), (e, 0,0,0)]

-- Time, Pontos, Saldo de Gols, Gols Feitos
contaPonto :: [Jogo] -> Tabela -> Tabela
contaPonto [] tabela = tabela
contaPonto ((a, b, c, d):xs) tabela
  | b > c = contaPonto xs (updateWin (a, b, c, d) tabela)
  | b < c = contaPonto xs (updateWin (d, c, b, a) tabela)
  | b == c = contaPonto xs (updateDraw (a, b, c, d) tabela)

-- Ordenação da Tabela Em ordem Decrescente

qSortTuple :: Tabela -> Tabela
qSortTuple [] = []
qSortTuple ((a, b, c, d):xs) = qSortTuple [(a, y, z, w) | (a, y, z, w) <- xs, y > b || (y == b && z > c) || ( y == b && z == c && w > d) ] ++ [(a, b, c, d)] ++ qSortTuple [(a, y, z, w) | (a, y, z, w) <- xs, y < b ||(y == b && z < c) || ( y == b && z == c && w < d)]

qSortSaldo :: Tabela -> Tabela
qSortSaldo [] = []
qSortSaldo ((a, b, c, d):xs) = qSortSaldo [(a, b, y, d) | (a, b, y, d) <- xs, y >= c] ++ [(a, b, c, d)] ++ qSortTuple [(a, b, y, d) | (a, b, y, d) <- xs, y < c]

qSortS :: Tabela -> Tabela
qSortS [] = []
qSortS ((time, p, sg, gf):xs)
  | p == getSecondFromTouple (head xs) = qSortSaldo((time, p, sg, gf):qSortS(xs))
  | otherwise = (time, p, sg, gf): (qSortSaldo xs)

getHeadPoints :: (Time, Int, Int, Int) -> Int
getHeadPoints (a,b,c,d) = b

getTwoFirst :: Tabela -> (Time, Time)
getTwoFirst (x:xs) = (getFirstFromTouple x, getFirstFromTouple (head xs) )

getFirstFromTouple :: (Time, Int, Int, Int) -> Time
getFirstFromTouple (a, b, c, d) = a

getSecondFromTouple :: (Time, Int, Int, Int) -> Int
getSecondFromTouple (a, b, c, d) = b

-- Teste:
-- classificados ('A', "Egito", "Russia", "Arabia", "Uruguai") jogos1
classificados :: Grupo -> [Jogo] -> Tabela
classificados x jogos = (qSortTuple (contaPonto (tratarJogos x jogos) (createTabelaFromGrupo x)))
-- contaPonto (tratarJogos groupA  jogos1) (createTabelaFromGrupo groupA)

