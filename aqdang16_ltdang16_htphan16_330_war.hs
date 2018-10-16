-- Anh Dang
-- Lam Dang
-- Huong Phan

import Control.Monad
import System.Random
import System.IO
import Data.List
-- Create Deck
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving (Eq,Ord,Show,Enum)

fulldeck :: [Card]
fulldeck = [Two .. Ace]++[Two .. Ace]++[Two .. Ace]++[Two .. Ace]
-- Shuffle Deck
shuffle x = if length x < 2 then return x else do
  i <- System.Random.randomRIO (0, length(x)-1)
  r <- shuffle (take i x ++ drop (i+1) x)
  return (x!!i : r)
-- AutoPlay mechanics
playAuto (p1,p2) = do
  putStrLn "Number of cards left is"
  print [("Player 1", length p1), ("Player 2", length p2)]
  -- Condition for winning and losing
  if length p1 == 0 then putStrLn "Player 2 Win" else
    if length p2 == 0 then putStrLn "Player 1 Win" else do
      -- Comparison with one card greater/smaller than another
      -- Determine the winner, take both card and continue the game
      putStrLn "Two fighting cards are"
      print [("Player 1", head p1), ("Player 2", head p2)]
      if (head p1 < head p2) then do
        putStrLn "Player 2 win this round"
        playAuto(tail p1, (tail p2) ++ [head p1, head p2]) else
        if (head p1 > head p2) then do
          putStrLn "Player 1 win this round"
          playAuto((tail p1) ++ [head p2, head p1], tail p2) else
            if (length p1 < 5) then do
            putStrLn "Player 2 win this round"
            playAuto([],p2++p1) else
            if (length p2 < 5) then do
              putStrLn "Player 1 win this round"
              playAuto(p1++p2,[]) else
      -- Draw. Initiate War
                war (p1,p2) 4
                where
                  war (l1,l2) x = do
                    putStrLn ""
                    putStrLn "Round draw, WAR!"
      -- Compare the number of cards at hand
      -- If less than the numbers of card needed to WAR then lose
                    if (length l1 < (x+1)) then do
                      putStrLn "Player 2 win the WAR"
                      putStrLn ""
                      playAuto([],l2 ++ l1) else
                        if (length l2 < (x+1)) then do
                          putStrLn "Player 1 win the WAR"
                          putStrLn ""
                          playAuto(l1++l2,[]) else do
      -- Compare the 4th card to determine the winner
                            putStrLn "Two fighting cards are"
                            print [("Player 1", l1 !! x), ("Player 2", l2 !! x)]
                            if ((l1 !! x) < (l2 !! x)) then do
                              putStrLn "Player 2 win the WAR"
                              putStrLn ""
                              playAuto(drop (x+1) l1, (drop (x+1) l2) ++ (take (x+1) l1) ++ (take (x+1) l2))
                              else if ((l1 !! x) > (l2 !! x)) then do
                                putStrLn "Player 1 win the WAR"
                                putStrLn ""
                                playAuto((drop (x+1) l1) ++ (take (x+1) l2) ++ (take (x+1) l1), drop (x+1) l2)
      -- If the 4th card are equal, initiate war again
                                else war (l1,l2) (x+4)
-- Start AutoWar
autoWar = do
  shuffledDeck <- (shuffle fulldeck)
  -- Divide shuffled full deck to two player
  let player1hand = take ((length shuffledDeck) `div` 2) shuffledDeck
      player2hand = drop ((length shuffledDeck) `div` 2) shuffledDeck
  -- Print hand
  print player1hand
  putStrLn ""
  print player2hand
  putStrLn ""
  -- Start game
  playAuto (player1hand,player2hand)

-- interactiveWar mechanic
playInteractive (p1,p2) = do
    putStrLn "Number of cards left is"
    print [("You", length p1), ("Computer", length p2)]
    -- Condition for winning and losing
    if length p1 == 0 then putStrLn "Computer Win" else
      if length p2 == 0 then putStrLn "You Win" else do
    -- Comparison with one card greater/smaller than another
    -- Determine the winner, take both card and continue the game
        putStrLn "Enter to continue"
        action <- getLine
        putStrLn "Two fighting cards are"
        print [("You", head p1), ("Computer", head p2)]
        if (head p1 < head p2) then do
          putStrLn "Computer win this round"
          playInteractive(tail p1, (tail p2) ++ [head p1, head p2]) else
          if (head p1 > head p2) then do
            putStrLn "You win this round"
            playInteractive((tail p1) ++ [head p2, head p1], tail p2) else
            if (length p1 < 5) then do
              putStrLn "Computer win this round"
              playInteractive([],p2++p1) else
              if (length p2 < 5) then do
                putStrLn "You win this round"
                playInteractive(p1++p2,[]) else
    -- Draw. Initiate War
                  war (p1,p2) 4
                  where
                    war (l1,l2) x = do
                      putStrLn ""
                      putStrLn "Round draw, WAR!"
    -- Compare the number of cards at hand
    -- If less than the numbers of card needed to WAR then lose
                      if (length l1 < (x+1)) then do
                        putStrLn "Computer win the WAR"
                        putStrLn ""
                        playInteractive([],l2 ++ l1) else
                          if (length l2 < (x+1)) then do
                            putStrLn "You win the WAR"
                            putStrLn ""
                            playInteractive(l1++l2,[]) else do
                              putStrLn "Enter to continue"
                              action <- getLine
                              putStrLn "Two fighting cards are"
                              print [("You", l1 !! x), ("Computer", l2 !! x)]
                              if ((l1 !! x) < (l2 !! x)) then do
                                putStrLn "Computer win the WAR"
                                putStrLn ""
                                playInteractive(drop (x+1) l1, (drop (x+1) l2) ++ (take (x+1) l1) ++ (take (x+1) l2))
      -- Compare the 4th card to determine the winner
                                else if ((l1 !! x) > (l2 !! x)) then do
                                  putStrLn "You win the WAR"
                                  putStrLn ""
                                  playInteractive((drop (x+1) l1) ++ (take (x+1) l2) ++ (take (x+1) l1), drop (x+1) l2)
      -- If the 4th card are equal, initiate war again
                                  else war (l1,l2) (x+4)
-- Start InteractiveWar
interactiveWar = do
  shuffledDeck <- (shuffle fulldeck)
-- Divide shuffled Deck to 2 players
  let player1hand = take ((length shuffledDeck) `div` 2) shuffledDeck
      player2hand = drop ((length shuffledDeck) `div` 2) shuffledDeck
  playInteractive(player1hand,player2hand)
