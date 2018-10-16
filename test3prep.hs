import System.Random
data MyExpression = I Int 
                  | Add MyExpression MyExpression
                  | Mult MyExpression MyExpression 
                  deriving (Eq, Show)

evalExp :: MyExpression -> Int
evalExp (I a) = a
evalExp (Add x y) = (evalExp x) + (evalExp y)
evalExp (Mult x y) = (evalExp x) * (evalExp y)


myRecs = 
  do
  	putStr "Please enter a food name: "
  	x <- getLine
  	let recs1 = ["Pizza", "Chicken"]
  	let recs2 = ["Cola", "Sprite"]
  	let recs3 = ["Hamburger", "Banana"]
  	if (elem x recs1 == True)
  	  then
  	  	do
  	  	  print "A Restaurant"
  	else if (elem x recs2 == True)
  	  then
  	  	do 
  	  	  print "B Restaurant"
  	else if (elem x recs3 == True)
  	  then
  	  	do 
  	  	  print "C Restaurant"
  	else do 
      	putStrLn "There's no recs. Please enter a different food name"
      	myRecs

b = System.Random.randomRIO (1, 100)

intGuessGame = 
  do
  	putStr "Please enter a number from 1 to 100: "
  	x <- getLine
  	let a = read x :: Int
  	if a > b
      then
  	    do 
  		  print "too high"
  		  intGuessGame
    else if a < b
      then
        do 
  		  print "too high"
  		  intGuessGame
    else do
      print "correct"

minInList :: Ord a => [a] -> a
minInList [x] = x
minInList (x:xs)
  | (x >= head xs) = minInList xs 
  | otherwise = minInList (x:(drop 1 xs))

abc :: [String] -> [String]
abc ls = [x++show a | a <- [1..10], x <- ls]

def :: [a] -> (a -> Bool) -> [a]
def ls f = [x | x <- ls, (f x) == True]

