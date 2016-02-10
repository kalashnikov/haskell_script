import Data.List
import Data.Char
import qualified Data.Map as Map

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

---------------------------
-- Ch.5 High-level function
---------------------------

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

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

---------------------------
------- Ch.6 Module -------
---------------------------

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort .words 

-- encode/decode

encode :: Int -> String -> String
-- encode offset msg = map (\c -> chr $ ord c + offset) msg
encode offset msg = map ( chr . (+ offset) . ord ) msg  

decode :: Int -> String -> String 
decode offset msg = encode (negate offset) msg

-- Maybe & find

digitSum :: Int -> Int 
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

db :: Map.Map Int String 
db = Map.fromList [(1,"Kala"), (2,"Ashley"),(3,"Mom")]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v 

-- findKey key [] = Nothing
-- findKey key ((k,v):xs)
--   | key == k = Just v
--   | otherwise = findKey key xs

findKey key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs


db2 = [(1,"Kala"), (2,"Ashley"), (3,"Mom"), (3,"Dad")]
-- db2Map :: (Ord k) => [(k, String)] -> Map.Map k String
-- db2Map xs = Map.fromListWith add xs 
--   where add num1 num2 = num1 ++ ", " ++ num2
db2Map :: (Ord k) => [(k, a)] -> Map.Map k [a] 
db2Map xs = Map.fromListWith (++) $ map (\(k,v) -> (k, [v])) xs 


---------------------------
-------- Ch.7 Data --------
---------------------------

data Point = Point Float Float deriving (Show) 
data Shape = Circle Point Float | Rectangle Point Point deriving (Show) 

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)


nudge :: Shape -> Float -> Float -> Shape
nudge ( Circle (Point x y) r ) a b = Circle (Point (x+a) (y+b)) r 
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape 
baseCircle r = Circle (Point 0 0) r 

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height) 


data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     , height    :: Float
                     , phoneNum  :: String 
                     , flavor    :: String } deriving (Show) 

data Car = Car { company :: String
               , model   :: String 
               , year    :: Int
               } deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


data LockerState = Taken | Free deriving (Show, Eq)

-- Alias
type Code = String 
type LockerMap = Map.Map Int (LockerState, Code) 

lockerLookup :: Int -> LockerMap -> Either String Code 
lockerLookup lockerNum map = case Map.lookup lockerNum map of 
    Nothing -> Left $ "Locker" ++ show lockerNum ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken 
                          then Right code 
                          else Left $ "Locker" ++ show lockerNum ++ " is already taken!"

lockers :: LockerMap 
lockers = Map.fromList 
  [(100, (Taken, "ZD39I"))
  ,(101, (Free, "JAH3I")) 
  ,(103, (Free, "IQSA9")) 
  ,(105, (Free, "QOTSA")) 
  ,(109, (Taken, "893JJ")) 
  ,(110, (Taken, "99292"))
  ]

--

main :: IO ()
main = print ( factorial 10 )
