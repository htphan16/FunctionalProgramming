import Data.Char 

-- Coding Assignment 3.5 
-- Define a function to convert lowercase letters to their uppercase counterparts. 
-- If the parameter is not a lowercase letter, it should be returned unchanged. Your function should not have 26 conditions.

lowerToUpper :: Char -> Char 
lowerToUpper ch = chr (ord (ch) + ord ('A') - ord ('a'))

-- Coding Assignment 3.6
-- Define a function charToNum :: Char -> Int which converts a digit like ’9’ into its value, 9. 
-- Your function should do something sensible if the input is not a digit. (Don’t use read for this.)


charToNum :: Char -> Int 
charToNum ch = if isDigit (ch) then ord (ch) - ord('0') else 0


-- Coding Assignment 3.7
-- Write tests and/or propositions for the previous two functions. Test your propositions with Quickcheck if you are able to do so.
-- Test code for 3.5
test_lowerToUpper1 = lowerToUpper ('B') == 'B'
test_lowerToUpper2 = lowerToUpper ('f') == 'F'
test_lowerToUpper3 = lowerToUpper ('r') == 'R'
test_lowerToUpper4 = lowerToUpper ('e') == 'E'
test_lowerToUpper5 = lowerToUpper ('G') == 'G'
test_lowerToUpper6 = lowerToUpper ('y') == 'Y'

-- Test code for 3.6
test_charToNum1 = charToNum ('2') == 2
test_charToNum2 = charToNum ('3') == 3
test_charToNum3 = charToNum ('a') == 0
test_charToNum4 = charToNum ('8') == 8
test_charToNum5 = charToNum ('9') == 9
test_charToNum6 = charToNum ('G') == 0


-- Coding Assignment 3.9
-- Define a function which takes three strings and returns a single string. 
-- When that string is printed, it shows the three input Strings on separate lines. (Your function should not print anything itself, just return a String.)

onThreeLines :: String -> String -> String -> IO ()
onThreeLines a b c = putStr(show a ++ "\n" ++ show b ++ "\n" ++ show c)

