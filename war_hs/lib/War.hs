module War (deal) where

import Data.List

{--
The War module uses three (main) functions deal, playGame and war to replicate the card game War.
The game starts by taking a list of 52 cards where the ace, jack, queen and king are equivalent to 1, 11, 12 and 13 respectively.
The deal/1 function takes the list of 52 cards and uses the Enum library to deal the cards to player1 and player2.

The playGame function takes the player1 and player2 decks and starts the game. It uses recursion to continuously compare the first values
of each deck and assign the winner of the round the cards, it also handles the case when one of the players wins the game and it returns the final
deck.

When a war occurs the war method is called it handles the case when the cards compared between the two players decks are the same. It takes the new face up
cards and face down cards from each deck and then based on the their values either calls the playGame function or recursively calls itself again.

--}
main :: IO ()
main = do
    putStrLn "Hello, world!"

--This function takes an array of integers and returns a modified version of that array based on how the card game plays out
deal :: [Int] -> [Int]
deal shuf =
    --converts all the 1's (aces) in the arrays to 14 so that they are in their correct position when sorting
    --To shuffel the deck uses the helper function everyOther to create a new array that holds every other element(all even elements) from the original array
    --then uses the every helper function to create a new array with all the odd elements from the original array
    --both lists are reversed and then to start the game calls the playGame function
    let shufTemp = map (\x -> if x == 1 then 14 else x) shuf
        p1 = reverse (everyOther shufTemp)
        p2 = reverse (every shufTemp)
    in playGame(p1, p2)

-- This function returns a list containing every other element in the input list.
every :: [a] -> [a]
every [] = []
every [x] = [x]
every (x:_:xs) = x : every xs

-- This function returns a list containing all even-indexed elements in the input list.
everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = []
everyOther (_:y:xs) = y : everyOther xs

-- This function simulates a game of War, given the current state of both players' decks.
playGame :: ([Int], [Int]) -> [Int]
-- If player 1 is out of cards, return player 2's deck (after converting any remaining aces to 1s).
playGame ([], p2) = map (\n -> if n == 14 then 1 else n) p2
--vice versa, player 2 is out of cards
playGame (p1, []) = map (\n -> if n == 14 then 1 else n) p1

-- If player 1's card is greater, add both cards to the bottom of player 1's deck.
-- If player 2's card is greater, add both cards to the bottom of player 2's deck.
-- If both players' cards are equal, initiate a "war" (call the war function) 
-- and add both cards to a list that holds the cards that the winner of the round will recieve after being sorted.
playGame (firstP1:p1Rest, firstP2:p2Rest) =
  case compare firstP1 firstP2 of
    GT -> playGame (p1Rest ++ [firstP1, firstP2], p2Rest)
    LT -> playGame (p1Rest, p2Rest ++ [firstP2, firstP1])
    EQ -> war p1Rest p2Rest [firstP1, firstP2]

-- This function simulates a "war", where both players' cards are equal.
-- It takes in the remaining cards in each player's deck, as well as the "war chest" (a list containing the cards that caused the war).
war :: [Int] -> [Int] -> [Int] -> [Int]

-- When both players have only one card left, it ends the game by calling the playGame function with an empty list indicating the game has ended
-- creates a new deck that includes the initial cards in the war chest as well as the last card played by each player.
war [lastP1] [lastP2] warChest =
  let deck = sortOn negate (warChest ++ [lastP1, lastP2])
  in playGame (deck, [])

 -- If the first player runs out of cards during a war, they lose and the remaining 
 -- cards in the war chest and the second player's deck are added to their opponent's deck. 
war [] p2 warChest = playGame ([], p2 ++ warChest)

--vice versa, second player runs out of cards
war p1 [] warChest = playGame (p1 ++ warChest, [])

-- If both players have at least three cards left, another war occurs. The top two cards 
-- are discarded and added to the war chest, and the third card is compared to determine the winner. If the third cards are equal, the war continues recursively.
war (secP1:thirdP1:p1Rest) (secP2:thirdP2:p2Rest) warChest =
  --based on who wins the round sorts all the used cards in descending order, within the warchest, and adds them to the correct players deck
  case compare thirdP1 thirdP2 of
    GT -> playGame (p1Rest ++ sortOn negate ([secP1, thirdP1, secP2, thirdP2] ++ warChest), p2Rest)
    LT -> playGame (p1Rest, p2Rest ++ sortOn negate ([secP1, thirdP1, secP2, thirdP2] ++ warChest))
    EQ -> war p1Rest p2Rest ([secP1, thirdP1, secP2, thirdP2] ++ warChest)

