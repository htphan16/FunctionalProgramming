-- Coding Assignment 5.5: (2 Points)

-- The running time will be long since there are two recursive functions that are repeated



-- Coding Assignment 5.6: (4 Points)

tribStep :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
tribStep (a, b, c) = (b, c, a+b+c) 

tribThree :: Integer -> (Integer, Integer, Integer)
tribThree n 
  | n == 0    = (0,0,1)
  | otherwise = tribStep (tribThree (n-1))

first :: (Integer, Integer, Integer) -> Integer
first (a,b,c) = a

fastTrib :: Integer -> Integer
fastTrib = first . tribThree


-- Coding Assignment 5.7: (8 Points)

data PlayingCard = Card Suit Value          
                   deriving (Eq, Show)
type Suit = String
type Value = String


beat :: PlayingCard -> PlayingCard -> Bool
beat (Card "Spades" _) (Card "Hearts" _) = True
beat (Card "Spades" _) (Card "Diamonds" _) = True
beat (Card "Spades" _) (Card "Clubs" _) = True
beat (Card "Hearts" _) (Card "Diamonds" _) = True
beat (Card "Hearts" _) (Card "Clubs" _) = True
beat (Card "Diamonds" _) (Card "Clubs" _) = True
beat (Card _ "Two") (Card _ "Ace") = True
beat (Card _ _) (Card _ _) = False

lose :: PlayingCard -> PlayingCard -> Bool
lose (Card "Hearts" _) (Card "Spades" _) = True
lose (Card "Diamonds" _) (Card "Spades" _)  = True 
lose (Card "Clubs" _) (Card "Spades" _) = True
lose (Card "Diamonds" _) (Card "Hearts" _) = True
lose (Card "Clubs" _) (Card "Hearts" _) = True
lose (Card "Clubs" _) (Card "Diamonds" _)  = True
lose (Card _ "Ace") (Card _ "Two")  = True
lose (Card _ _) (Card _ _) = False



-- Coding Assignment 5.9: (2 Points) 

data AnyCard = PlayingCard String String | Joker String
               deriving (Eq, Show)


-- Coding Assignment 5.10: (4 Points)
-- The following should be done with list comprehensions.
-- a) Give a definition of a function
-- doubleAll :: [Integer] -> [Integer]
-- which doubles all of the elements of a list of integers and returns the result.

doubleAll :: [Integer] -> [Integer]
doubleAll a = [2*x | x <- a]

-- b) Define the function
-- divisors :: Integer -> [Integer]
-- which returns a list of all of the integer divisors of a positive integer you pass to it. For example, 
-- divisors 12
-- Should return [1,2,3,4,6,12]
-- Hint: You may find the notation introduced in the bottom half of page 110 useful.
divisors :: Integer -> [Integer]
divisors a = [x | x <- [1..], (mod a x == 0)]

