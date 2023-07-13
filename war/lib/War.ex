defmodule War do
  @moduledoc """
  The War module uses three functions deal/1, playGame/2 and war/3 to replicate the card game War.
  The game starts by taking a list of 52 cards where the ace, jack, queen and king are equivalent to 1, 11, 12 and 13 respectively.
  The deal/1 function takes the list of 52 cards and uses the Enum library to deal the cards to player1 and player2.

  The playGame/2 function takes the player1 and player2 decks and starts the game. It uses recursion to continuously compare the first values
  of each deck and assign the winner of the round the cards, it also handles the case when one of the players wins the game and it returns the final
  deck.

  When a war occurs the war/3 method is called it handles the case when the cards compared between the two players decks are the same. It takes the new face up
  cards and face down cards from each deck and then based on the their values either calls the playGame/2 function or recursively calls itself again.

  Documentation for `War`.
  """

  def deal(shuf) do

    # map all 1s in the deck to 14s, helps keep the 1 (ace) at the top when sorting
    shuf = shuf |> Enum.map(fn x -> if x == 1, do: 14, else: x end)

    # create the first player's deck
    p1 = Enum.with_index(shuf)
    # include only cards with even indexes
      |> Enum.map(fn{value, index} ->
        if rem(index, 2) == 0 do
          value
        end
      end)

    # create the second player's deck
    p2 = Enum.with_index(shuf)
    # include only cards with odd indexes
      |> Enum.map(fn{value, index} ->
        if rem(index, 2) == 1 do
          value
        end
      end)


    # filter out nil values and reverse the order of the decks (as the cards originally at the top should now be at the bottom afer dealing them)
    p1 = Enum.filter(p1, fn(n)-> n != nil end)
    p1 = Enum.reverse(p1)
    p2 = Enum.filter(p2, fn(n)-> n != nil end)
    p2 = Enum.reverse(p2)

    playGame(p1, p2)
  end



# This function takes in two lists of integers representing the cards for two players and simulates a game of war.
def playGame(p1, p2) do


  case {p1, p2} do
     # Case when player 1 has no cards left Player 2 wins the game
      {[], _} ->
          p2 |> Enum.map(fn n -> if n == 14, do: 1, else: n end)

      # Case when player 2 has no cards left Player 1 wins the game
      {_, []} ->
          p1 |> Enum.map(fn n -> if n == 14, do: 1, else: n end)

      # Case when both players can still play the game
      {[firstP1 | p1Rest], [firstP2 | p2Rest]} ->
          # Compare the top cards of each player's deck
          cond do
              #when a player whens the round adds the compared cards to their deck in the correct order (highest first)
              #and recursively calls the function again with the new decks
              firstP1 > firstP2 ->
                  p1New = p1Rest ++ [firstP1, firstP2]
                  playGame(p1New, p2Rest)

              firstP1 < firstP2 ->
                  p2New = p2Rest ++ [firstP2, firstP1]
                  playGame(p1Rest, p2New)

              true ->
                  # There is a tie, so a war must occur, takes both decks without their first card and adds them to their own list
                  war(p1Rest, p2Rest, [firstP1, firstP2])
          end
    end
end

  def war(p1, p2, warChest) do


    case {length(p1), length(p2)} do
      #when there is a tie
      {1, 1} ->
        # combines the warChest and both players' cards into a new deck and sorts them all then calls the playGame function to end the game
        deck = warChest ++ p1 ++ p2
        |> Enum.sort()
        |>Enum.reverse()
        playGame(deck, [])

       # if player 1 has no cards left, player 2 wins the game with the current warChest
      {0, _} ->
        playGame([], p2 ++ warChest)

      # if player 2 has no cards left, player 1 wins the game with the current warChest
      {_, 0} ->
        playGame(p1 ++ warChest, [])

      _ ->
        #gets the next face down and face up card (respectively) from both players decks
        [secP1, thirdP1 | p1Rest] = p1
        [secP2, thirdP2 | p2Rest] = p2

        cond do
          #if player 1 wins, sort the cards, add them to player1's deck and continue the game with the new decks
          thirdP1 > thirdP2 ->
            sortP1 = Enum.sort([secP1, thirdP1, secP2, thirdP2] ++ warChest)
            sortP1 = Enum.reverse(sortP1)
            p1Rest = p1Rest ++ sortP1
            playGame(p1Rest, p2Rest)

          #if player 2 wins, sort the cards, add them to player2's deck and continue the game with the new decks
          thirdP1 < thirdP2 ->
            sortP2 = Enum.sort([secP1, thirdP1, secP2, thirdP2] ++ warChest)
            sortP2 = Enum.reverse(sortP2)
            p2Rest = p2Rest ++ sortP2
            playGame(p1Rest, p2Rest)

          #if another tie continue the war and add all the cards that have been used in the round so far to the warChest
          true ->
            war(p1Rest, p2Rest, [secP1, thirdP1, secP2, thirdP2] ++ warChest)
        end
    end
  end

end
