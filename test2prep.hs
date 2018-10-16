import Data.List
import Data.Char
import Control.Monad
import Prelude

greaterThan7 :: [Int] -> [Bool]
greaterThan7 [] = []
greaterThan7 ls = map greater ls
  where greater a = a > 7

--greaterThan7 ls = map (>7) ls

--greaterThan7 ls = map (\a -> a > 7) ls

takeNMachine :: Int -> ([Int] -> [Int])
takeNMachine a = (\l -> take a l)

--doubleInt = 
--  do
--    putStrLn "Please enter an integer: "
--    x <- getLine
--    if (x == "")
--     then 
--      	do 
--      	  return ()
--      else
--        do 
--          print (2 * (read x))
--          doubleInt

highestDigit :: String -> Int
highestDigit a 
  | (filter isDigit a == "") = -1
  | otherwise = maximum (map digitToInt (filter isDigit a))

highestDigit2 :: String -> Maybe Int
highestDigit2 a 
  | (filter isDigit a == "") = Nothing
  | otherwise = Just (maximum (map digitToInt (filter isDigit a)))

myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
myZipWith3 f [] _ _ = []
myZipWith3 f _ [] _ = []
myZipWith3 f _ _ [] = []
myZipWith3 f (x:xs) (y:ys) (z:zs) = (f x y z) : (myZipWith3 f xs ys zs)

