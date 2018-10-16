-- Coding Assignment 4.1

-- The notTen function returns False if variable is set to 10, and returns True when variable is set to any integers other than 10

import PicturesSVG

notTen :: Integer -> Bool
notTen 10 = False
notTen n = True

-- The implication function returns True and False values according to logical implication (p => q) rule 

implication :: Bool -> Bool -> Bool
implication True True = True
implication True False = False
implication False True = True
implication False False = True

-- Coding Assignment 4.3:

fourPics1 :: Picture -> Picture

fourPics1 pic = 
	top `above` bottom
	where
		top = pic `beside` invertColour (flipV pic)
	  	bottom = invertColour (pic) `beside` invertColour (flipV pic)
	

fourPics2 :: Picture -> Picture

fourPics2 pic = 
	top `above` bottom
	where
		top = pic `beside` invertColour (flipV pic)
	  	bottom = invertColour (top)

-- Coding Assignment 4.4:

data Sports = Football | Soccer | Basketball
			  deriving (Eq)

isCoolerThan :: Sports -> Sports -> Bool
isCoolerThan Football Soccer = True
isCoolerThan Soccer Basketball = True
isCoolerThan Basketball Basketball = True
isCoolerThan m n = False

canUseHands :: Sports -> String
canUseHands Football = "Yes!"
canUseHands Soccer = "No!"
canUseHands Basketball = "Yes!"

-- Coding Assignment 4.5:

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n 
  | n < m = 0
  | n > m = ((rangeProduct m (n - 1)) * n)
  | otherwise = m

-- Coding Assignment 4.6:

recurseMult :: Integer -> Integer -> Integer
recurseMult m n 
  | (n == 0 && m == 0) = 0
  | m > 0 = n + recurseMult (m-1) n
  | otherwise = 0