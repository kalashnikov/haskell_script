
doubleMe x = x + x 

doubleUs x y = x * 2 + y * 2 

doubleSmallNumber x = if x > 100 
                      then x 
                      else x*2 

circum :: Float -> Float 
circum r = 2 * pi * r 

circum' :: Double -> Double 
circum' r = 2 * pi * r 

fact :: Integer -> Integer 
fact n = product [1..n] 

--------------------------
-- Ch.3 Function syntax --
--------------------------

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial(n-1)

head' :: [a] -> a 
head' [] = error "Can't call head on empty list." 
head' (x:_) = x

firstLetter' :: String -> String 
firstLetter' "" = "Empty string." 
firstLetter' all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height 
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Fat"
  | otherwise = "Whale"
  where bmi = weight / height ^ 2 

initials :: String -> String -> String 
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname 
        (l:_) = lastname

describeList :: [a] -> String
describeList ls = "This list is " ++ case ls of [] -> "empty." 
                                                [x] -> "a singleton list."
                                                xs -> "a longer list."
--------------------
-- Ch.4 Recursion --
--------------------

max' :: (Ord a) => [a] -> a
max' [] = error "max of empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)


rep' :: Int -> a -> [a] 
rep' n x 
  | n <= 0 = []
  | otherwise = x:rep' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' n [] = []
take' n (x:xs) = x:take' (n-1) xs


-- reverse' :: [a] -> [a] 
-- reverse' [] = []
-- reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: n -> [n] 
repeat' x = x:repeat' x


zip' :: [a] -> [b] -> [(a,b)] 
zip' _ [] = [] 
zip' [] _ = [] 
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- elem' :: (Eq a) => a -> [a] -> Bool
-- elem' a [] = False
-- elem' a (x:xs) 
--   | a == x    = True
--   | otherwise = a `elem'` xs 

quicksort :: (Ord a) => [a] -> [a] 
quicksort [] = [] 
quicksort (x:xs) = 
  let smallOrEuqal = [a | a <- xs, a <= x ]
      larger       = [a | a <- xs, a  > x ]
  in quicksort smallOrEuqal ++ [x] ++ quicksort larger

-- Ch.5 High-level function

applyTwice :: (a -> a) -> a -> a 
applyTwice f x = f (f x) 

zipWith' :: ( a -> b -> c ) -> [a] -> [b] -> [c] 
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c 
flip' f y x = f x y

-- map' :: (a->b) -> [a] -> [b] 
-- map' _ [] = [] 
-- map' f (x:xs) = f x : map f xs

-- filter' :: (a->Bool) -> [a] -> [a]
-- filter' _ [] = []
-- filter' p (x:xs)
--   | p x       = x : filter' p xs 
--   | otherwise = filter' p xs


chain :: Integer -> [Integer] 
chain 1 = [1] 
chain n 
  | even n = n:chain (n `div` 2)
  | odd  n = n:chain (n*3  +  1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]) ) 
  where isLong xs = length xs > 15

-- length ( filter (\xs -> length xs > 15) ( map chain [1..100]) )

-- foldl/foldr

sum' :: (Num a) => [a] -> a 
-- sum' xs = foldl (\acc x->acc + x) 0 xs 
sum' = foldl (+) 0 

map' :: (a->b) -> [a] -> [b]
-- foldr, acc is at right side, and use ':' to append
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- foldl, acc is at left side, and use '++' to append, not as fast as ':' 
-- Also, foldl cannot handle infinity list
-- map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

reverse' :: [a] -> [a] 
-- reverse' = foldl (\acc x -> x : acc) [] 
reverse' = foldl (flip (:)) [] 
-- reverse' [3,4,5,6]
-- flip (:) (flip (:) (flip(:) (flip(:) [] 3) 4) 5) 6

filter' :: (a->Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) [] 

last' :: [a] -> a 
last' = foldl1 (\_ x -> x)

sqrtSums :: Int 
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<1000) . filter odd $ map (^2) $ [1..]  

-- Ch.6


main :: IO ()
main = print ( factorial 10 )
