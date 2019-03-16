-- Questão 1 -  AP 1
-- [IF686EC] - 2019.1
-- autor: tasm2

split :: String -> [String]
split []  = [""]
split (c:cs)
   | c == ';' || c == ' ' = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs

logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

strToDouble :: String -> Double
strToDouble xs = read xs :: Double

logMesSplit :: String -> [String] -> Double
logMesSplit _ [] = 0
logMesSplit mes dataLog
 | mes == head dataLog = strToDouble (dataLog!!2) + logMesSplit mes cauda
 | otherwise = logMesSplit mes cauda
 where
   cauda = tail dataLog

logMes:: String -> String -> Double
logMes a b = logMesSplit a (split b)
