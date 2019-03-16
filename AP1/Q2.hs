-- Questão 2 -  AP 1
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

-- Support functions
strToDouble :: String -> Double
strToDouble xs = read xs :: Double

menor :: Double -> Double -> Double
menor a b
 | a <= b = a
 | otherwise = b

maior :: Double -> Double -> Double
maior a b
 | a >= b = a
 | otherwise = b

inf = 1/0
infNegative = -1/0

-- Recursively get only the cost values
dataLogSplit :: [String] -> [String]
dataLogSplit [] = []
dataLogSplit dataLog
 | length dataLog <= 4 = []
 | otherwise = dataLog!!3 : dataLogSplit (tail (tail (tail (tail dataLog))))

-- Transform the list into doubles
dataLogToDouble xs = [strToDouble x | x <- dataLogSplit (split xs)]

minMaxCartaoSplit :: [Double] -> (Double, Double)
minMaxCartaoSplit [] = (inf, infNegative)
minMaxCartaoSplit dataLog = (menor cabeca  (fst resto), maior cabeca (snd resto))
  where
    cabeca = head dataLog
    cauda = tail dataLog
    resto = minMaxCartaoSplit cauda

minMaxCartao :: String -> (Double, Double)
minMaxCartao a = minMaxCartaoSplit (dataLogToDouble a)
