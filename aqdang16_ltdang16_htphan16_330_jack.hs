-- Anh Dang
-- Lam Dang
-- Huong Phan

import Control.Monad
import System.Random
import System.IO
import Data.List
-- Create Deck
data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq,Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving (Eq,Ord,Show)

data PlayingCard = Card Rank Suit
            deriving(Eq,Show)

deck :: [PlayingCard]
deck = [Card rank suit| rank <- [Two , Three , Four , Five , Six , Seven , Eight , Nine , Ten , Jack , Queen , King , Ace],suit <- [Clubs , Diamonds , Hearts , Spades]]
-- Shuffle Deck
deckShuffled :: IO [PlayingCard]
deckShuffled = shuffle deck

shuffle x = if length x < 2 then return x else do
  i <- System.Random.randomRIO (0, length(x)-1)
  r <- shuffle (take i x ++ drop (i+1) x)
  return (x!!i : r)

getRank :: PlayingCard -> Rank
getRank (Card r s) = r
-- Convert card into value
convert :: PlayingCard -> [Int]
convert (Card rank suit)
  | rank == Two = [2]
  | rank == Three = [3]
  | rank == Four = [4]
  | rank == Five = [5]
  | rank == Six = [6]
  | rank == Seven = [7]
  | rank == Eight = [8]
  | rank == Nine = [9]
  | (rank == Ten || rank == Jack || rank == Queen || rank == King)  = [10]
  | rank == Ace = [1,11]
-- Deal 1 card
dealcards :: Int -> [PlayingCard] -> ([PlayingCard], [PlayingCard])
dealcards number deck = (take number deck, drop number deck)
--Iterate through hand, add value
possibleHandTotals :: [PlayingCard] -> [Int] -> [Int]
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) runningTotals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- runningTotals, value <- convert card]
-- Get the current score on your hand
handScore :: [PlayingCard] -> Int
handScore hand
  | (head (possibleHandTotals hand [0])) >21 = head (possibleHandTotals hand [0])
  | otherwise = last (filter (<=21) (possibleHandTotals hand [0]))
-- Check if BlackJack
isBlackjack :: [PlayingCard] -> Bool
isBlackjack [card1, card2] =
  (((getRank card1) == Ace) && elem (getRank card2) [Ten, Jack, Queen, King]) ||
  (((getRank card2) == Ace) && elem (getRank card1) [Ten, Jack, Queen, King])
isBlackjack _ = False

userTurn playerhand dealerhand dk = do
  putStrLn ""
  putStrLn "Your current hand is"
  print playerhand
  -- Check both hand for BlackJack
  if ((isBlackjack playerhand) && (isBlackjack dealerhand)) then do
    putStrLn "Dealer hand is"
    print dealerhand
    putStrLn "Both BlackJack! It's a tie" else
    if isBlackjack playerhand then do
      putStrLn "Got BlackJack! You Win"
      putStrLn "Dealer hand is"
      print dealerhand else
      if isBlackjack dealerhand then do
        putStrLn "Dealer hand is"
        print dealerhand
        putStrLn "Dealer BlackJack! You Lose" else
  -- Calculate current score
        if handScore playerhand == 21 then dealerTurn dealerhand dk 21
          else if handScore playerhand > 21 then do putStrLn "Bust! You Lose"
            else do
  -- Hit or Stand
              putStrLn "Hit? (Y/N)"
              decision <- getLine
              if ((decision == "Y") || (decision == "y")) then do
                let (newcard, remain) = dealcards 1 dk
                userTurn (playerhand ++ newcard) dealerhand remain
                else dealerTurn dealerhand dk (handScore playerhand)
-- Dealer's hand
dealerTurn hand dk userscore = do
  if (handScore hand) < 17 then do
    -- automatically add a new card
    let (newcard, remain) = dealcards 1 dk
    dealerTurn (hand ++ newcard) remain userscore
    -- Dealer hand is greater than 21
    else if (handScore hand) > 21 then do
      putStrLn ""
      putStrLn "Dealer hand is"
      print hand
      putStrLn "Dealer Bust! You Win"
    -- Compare dealer and player hand
      else if (handScore hand) > userscore then do
        putStrLn ""
        putStrLn "Dealer hand is"
        print hand
        putStrLn "You Lose"
        else if (handScore hand) < userscore then do
          putStrLn ""
          putStrLn "Dealer hand is"
          print hand
          putStrLn "You Win"
          else do
            putStrLn ""
            putStrLn "Dealer hand is"
            print hand
            putStrLn "It's a tie"
-- Start the game
blackjack = do
  deck2 <- deckShuffled -- Shuffle Deck
  -- Deal 2 card for each player
  let (playerHand, remainingDeck) = dealcards 2 deck2
      (dealerHand, remainingDeck') = dealcards 2 remainingDeck
  putStrLn "One card in dealer hand is"
  -- Show dealer's 1st card
  print (take 1 dealerHand)
  userTurn playerHand dealerHand remainingDeck'
