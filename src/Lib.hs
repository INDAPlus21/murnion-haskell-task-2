module F1 where
import Data.Char

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
fib 0 = 0
fib n = fibs !! (n-1)

consonant = "aeiouy"
rovarsprak :: [Char] -> [Char]
rovarsprak x = concatMap (\c -> if isConsonant c then [c, 'o', c] else [c]) x
isConsonant :: Char -> Bool
isConsonant c = not (elem c consonant)

karpsravor :: [Char] -> [Char]
karpsravor "" = ""
karpsravor x = if isConsonant (head x) then [head x] ++ karpsravor (drop 2 (tail x)) else [head x] ++ karpsravor(tail x)

alphabet = "abcdefghijklmnopqrstuvwxyz"
medellangd :: [Char] -> Double
medellangd "" = 0
medellangd x = (fromIntegral (length (filter isAlpha x))) / (fromIntegral (length (filter (not . isEmpty) (split x))))

isEmpty :: String -> Bool
isEmpty "" = True
isEmpty x = False

split :: String -> [String]
split [] = [""]
split (c:cs) | (not . isAlpha) c = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

varannan :: [x] -> [x]
varannan [] = []
varannan (x:xs) = if (length xs == 0) then [x] else [x] ++ varannan (tail xs)

notVarannan :: [x] -> [x]
notVarannan [] = []
notVarannan (x:xs) = if (length xs == 0) then [] else [head xs] ++ notVarannan (tail xs)

skyffla :: [x] -> [x]
skyffla [] = []
skyffla [x] = [x]
skyffla x = varannan x ++ skyffla (notVarannan x)
