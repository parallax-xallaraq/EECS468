-- Author: Thresa Kelly.
-- Date: 12/5/23.
-- File: EECS 468 Assignment 9.
-- Description: The game of nim.
-- Inputs: Console user imput.
-- Outputs: Nim game turns.
-- Sources: Referenced the lecture slides. Replit AI.
-- Replit: https://replit.com/join/whjydunbka-polaris133pc

module Main where

import Data.Char (digitToInt, isDigit)

main = do
  putStrLn "Thresa Kelly - Assignment 9. Enter 'nim' to start!"

----------------------------
-- USER INPUT
----------------------------

-- asks the user for a signle digit character, and converts to int
getDigit :: IO Int
getDigit = do
  digStr <- getLine -- get string from user
  if null digStr -- no input
    then getDigit
    else -- yes user gave input
      if length digStr > 1 || not (isDigit (head digStr)) -- input must be a single digit char
        then do
          -- print error and recursion
          putStrLn "ERR: Enter a single digit."
          getDigit
        else return (digitToInt (head digStr))

-- asks the user for a row
getRow :: IO Int
getRow = do
  putStr "Enter a row number: " -- screen print
  row <- getDigit -- get string from user
  if row > 0 && row <= 5 -- check for row<=5
    then return row -- finish
    else do
      -- print error and recursion
      putStrLn "ERR: Row is invalid. Try again."
      getRow

-- get number of stars to remove from user
getStars :: IO Int
getStars = do
  putStr "Stars to remove: " -- screen print
  stars <- getDigit -- user input
  if stars < 1 -- must give 1 or higher
    then do
      -- show error message and recursion to prompt again
      putStrLn "ERR: Stars must be a positive integer. Try again."
      getStars
    else return stars -- return number of stars

----------------------------
-- GAME BOARD
----------------------------

-- Board is a tuple of 5 Ints, where the index is the row and the value is the number of stars
type Board = (Int, Int, Int, Int, Int) -- (b1, b2, b3, b4, b5)

-- get starting game board
startBoard :: Board
startBoard = (5, 4, 3, 2, 1)

-- Build string of n stars with spaces between
starsStr :: Int -> String
starsStr n = concat (replicate n "* ")

-- display board to screen
showBoard :: Board -> IO ()
showBoard (b1, b2, b3, b4, b5) = do
  putStrLn ("1: " ++ starsStr b1) -- concatenate "row number : stars"
  putStrLn ("2: " ++ starsStr b2)
  putStrLn ("3: " ++ starsStr b3)
  putStrLn ("4: " ++ starsStr b4)
  putStrLn ("5: " ++ starsStr b5)

-- True if the Board has an item <1, False otherwise
isEndBoard :: Board -> Bool
isEndBoard (b1, b2, b3, b4, b5) = not (null [b | b <- [b1, b2, b3, b4, b5], b < 1]) -- get list of <1 values then check if list is empty.

-- subtract stars from a row in a Board
subBoard :: Int -> Int -> Board -> Board
subBoard row stars (b1, b2, b3, b4, b5)
  | row == 1 = (b1 - stars, b2, b3, b4, b5) -- subtract number of stars from 1st row
  | row == 2 = (b1, b2 - stars, b3, b4, b5) -- " 2nd row
  | row == 3 = (b1, b2, b3 - stars, b4, b5)
  | row == 4 = (b1, b2, b3, b4 - stars, b5)
  | row == 5 = (b1, b2, b3, b4, b5 - stars)
  | otherwise = (b1, b2, b3, b4, b5) -- invalid row, return original board

----------------------------
-- PLAYER
----------------------------

-- prints "Player n" to screen
showPlayer :: Char -> IO ()
showPlayer n = do putStrLn ("Player " ++ [n])

-- change between player 1 and 2
togglePlayer :: Char -> Char
togglePlayer player = if player == '1' then '2' else '1' -- 1 or 2

----------------------------
-- GAME
----------------------------

-- perform one turn n the game: show board and player, ask for row and stars to remove, update board
turn :: Char -> Board -> IO Board
turn player board = do
  showBoard board -- display board
  showPlayer player -- show player
  row <- getRow -- get row to remove stars from
  stars <- getStars -- get number of stars to remove
  return (subBoard row stars board) -- subtract stars from row

-- recursive nim game loop
play :: Char -> Board -> IO ()
play player board =
  if isEndBoard board -- check if end game
    then do putStrLn ("!! Player " ++ [player] ++ " wins !!") -- show game winner. End of recursion
    else do
      -- game not finished
      newBoard <- turn player board -- play a turn turn
      play (togglePlayer player) newBoard -- switch player and recursion

-- start game of nim
nim :: IO ()
nim = do play '1' startBoard -- start game