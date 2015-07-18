import Data.List

-- Brute force .... damn slow... 
fra :: [(Int,Int)]
fra = [(a,b) |  b<-[2..8000],a<-[1..b-1], a `rem` b /=0, gcd a b ==1 ]

-- https://wiki.haskell.org/99_questions/Solutions/35
primeFactors n = factor primes n
  where 
    factor ps@(p:pt) n | p*p > n      = [n]               
                       | rem n p == 0 = p : factor ps (quot n p) 
                       | otherwise    =     factor pt n
    primes = primesTME

-- https://wiki.haskell.org/99_questions/Solutions/31 
{-# OPTIONS_GHC -O2 -fno-cse #-}
-- tree-merging Eratosthenes sieve
--  producing infinite list of all prime numbers
primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs

-- https://wiki.haskell.org/99_questions/Solutions/35 
factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
  in let prime = if null divisors then n else head divisors
  in (prime :) $ factor $ div n prime

-- https://wiki.haskell.org/99_questions/Solutions/36 
prime_factors_mult = map encode . group . primeFactors
    where encode xs = (head xs, length xs)

-- Euler 72 
-- http://oeis.org/A000010
-- http://mathworld.wolfram.com/TotientFunction.html 
-- https://wiki.haskell.org/99_questions/Solutions/37 
-- http://mathworld.wolfram.com/RelativelyPrime.html
totient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]

totient_sum a = foldl1 (\acc x -> acc + (totient x) ) [1..a]

main = do 
  print (totient_sum 1000000) -- minus one will be the answer, rumtime: 3s 
