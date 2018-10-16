import Data.Char

firstHalf :: Char -> Bool
firstHalf ch
  | 0 <= x && x <= 12 = True 
  | otherwise = False
    where 
      x = fromEnum 'm' - fromEnum (toLower(ch))

firstHalfInt :: Bool -> Integer
firstHalfInt n
  | n == True = 1
  | n == False = 0

countEarliesR :: String -> Integer
countEarliesR n
  | (length n == 1) = firstHalfInt (firstHalf (head n))
  | (length n > 1) =  firstHalfInt (firstHalf (head n)) + countEarliesR (tail n)
  | otherwise = 0

countEarliesLC :: String -> Int
countEarliesLC n = length [x | x <- n, firstHalf x == True]

rectangle :: [(Integer, Integer, Integer, Integer)]
rectangle = [(a, b, c, d) | a <- [1..10], b <- [1..10], c <- [1..10], d <- [1..10], (a == b && c == d) || (a == c && b == d) || (a == d && b == c)]

humanNames :: [String] -> [String] -> [String]
humanNames a b = [x ++ y | x <- a, y <- b]

vowelCount :: String -> Int
vowelCount n = length [x | x <- n, elem x ['a', 'e', 'i', 'o', 'u']]

intersection :: [Integer] -> [Integer] -> [Integer]
intersection n1 n2 = [x | x <- n1, elem x n2]

palindromes :: [String] -> [String]
palindromes a = [x | x <- a, x == reverse x]
