-- Coding Assignment 1.4

-- A function to double a float.
double :: Float -> Float
double a = a*2

-- A function to find product of an integer with the integer right before.
multiply :: Integer -> Integer
multiply b = b*(b-1)

-- Function composition of the above two functions
comp :: Float
comp =  double (5) - multiply (3)

-- Coding Assignment 1.8

-- A function to triple an integer.
triple :: Integer -> Integer
triple c = c*3

-- A function to find sum of an integer with the integer right before.
add :: Integer -> Integer
add x = x+x-1

-- Function composition of the above two functions
result :: Integer
result = triple (3) * add(10)




