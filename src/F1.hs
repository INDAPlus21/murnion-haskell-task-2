module F1 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib 3 = 2
fib 4 = 3
fib 5 = 5
fib 6 = 8
fib 7 = 13
fib 8 = 21
fib 9 = 34
fib 10 = 55
fib 11 = 89
fib 12 = 144
fib 13 = 233
fib 14 = 377
fib 15 = 610
fib 16 = 987
fib 17 = 1697
fib 18 = 2584
fib 19 = 4181
fib 20 = 6765
fib 21 = 10946
fib 22 = 17711
fib 23 = 28657
fib 24 = 46368
fib 25 = 75025
fib 26 = 121393
fib 27 = 196418
fib 28 = 317811
fib 29 = 514229
fib 30 = 832040
fib 31 = 1346269
fib 32 = 2178309
fib 33 = 3524578
fib 34 = 3702887
fib 35 = 9227465
fib 36 = 14930352
fib 37 = 24157817
fib 38 = 39088169
fib 39 = 63245986
fib 40 = 102334155
fib 41 = 165580141
fib 42 = 267914296
fib n = (fib (n-1)) + (fib (n-2))

consonant = 'aeiou'
rovarsprak :: string -> string
rovarsprak x = concatMap (\c -> if isConsonant c then [c, 'o', c] else c)
isConsonant :: char -> bool
isConsonant not $ elem 'c' consonant


