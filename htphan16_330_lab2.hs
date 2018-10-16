--Tech Issues: I couldn't do cabal install, it gives me an error 
--As a rusult, I couldn't do any QuickCheck and I don't know if any of my code actually works
--Coding Assignment 2.4
--A
module UsePictures where
import Pictures

rotateHorse :: Picture
rotateHorse = flipH (flipV horse)

whiteHorse :: Picture 
whiteHorse = invertColour horse

--B
first, second1, second2, second3, picture1, picture2, picture3 :: Picture
first = beside horse whiteHorse

second1 = beside whiteHorse horse 

picture1 = above first1 second1

second2 = beside flipV (whiteHorse) flipV (horse) 

picture2 = above first1 second2

second3 = beside flipH (flipV whiteHorse) rotateHorse

picture3 = above first1 second3

--Coding Assignment 2.5

evilTwin :: Picture
evilTwin = flipV (invertColor horse)

-- Test for evilTwin
test_evilTwin1 = evilTwin (flipV (invertColor horse)) == horse
test_evilTwin2 = evilTwin (flipH (invertColor horse)) == rotatehorse
test_evilTwin3 = evilTwin (flipV horse) == whiteHorse






