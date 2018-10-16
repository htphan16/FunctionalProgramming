-- Coding Assignment 6.5
-- Assuming list is non-empty

myfoldr1 :: (a -> a -> a) -> [a] -> a
myfoldr1 f [x]     =  x
myfoldr1 f (x:xs)  =  f x (myfoldr1 f xs)

-- Coding Assignment 6.6

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f y [] = y
myfoldr f y [x] = f x y
myfoldr f y (x:xs) = f x (myfoldr f y xs)

-- Coding Assignment 6.7

myFilterA :: [a] -> (a -> Bool) -> [a]
myFilterA [] f = []
myFilterA (x:xs) f 
  | (f x == True) = x : (myFilterA xs f)
  | otherwise = myFilterA xs f


myFilterB :: [a] -> (a -> Bool) -> [a]
myFilterB [] f = []
myFilterB x f = [a | a <- x, f a == True]

-- Coding Assignment 6.8

titleMachine :: String -> (String -> String)
titleMachine n = (\m -> n ++ " " ++ m)

-- Coding Assignment 6.9
binaryArgFlip :: (a -> b -> c) -> (b -> a -> c)
binaryArgFlip f x y = f y x

-- Coding Assignment 6.11

totalLA :: (Integer -> Integer) -> (Integer -> Integer)
totalLA f n
  | (n == 0) = (\n -> f n) n
  | (n > 0) = (\n -> f n + totalLA f (n-1)) n
  
totalPA :: (Integer -> Integer) -> (Integer -> Integer)
totalPA f n
  | (n == 0) = f n
  | (n > 0) = f n + totalPA f (n-1)


-- Coding Assignment 6.12

curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
curry3 g x y z = g (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f (x, y, z) = f x y z
