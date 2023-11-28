-- Author: Thresa Kelly.
-- Date: 11/27/23.
-- File: EECS 468 Assignment 8.
-- Description: parse and evaluate arithmetic expressions containing operators +, -, *, /, %, and **, as well as numeric constants. The program should be able to handle expressions with parentheses to define precedence and grouping.
-- Inputs: parse "arithemtic equation"
-- Outputs: result of the equation
-- Sources: Referenced the lecture slides. Stack Overflow. Replit AI.
-- Replit: https://replit.com/join/hmeptbcgjt-polaris133pc

module Main where

import qualified Control.Exception as Exc

main = do
  putStrLn "Starting Thresa_Kelly_Assignment8..."
  print ("== Valid Expressions ==")
  print ("1. Addition: 3 + 4 --> " ++ (parse "3 + 4"))
  print ("2. Subtraction with Parentheses: 8 - (5 - 2) --> " ++ (parse "8 - (5 - 2)"))
  print ("3. Multiplication and Division: 10 * 2 / 5 --> " ++ (parse "10 * 2 / 5"))
  print ("4. Exponentiation: 2 ^ 3 --> " ++ (parse "2 ^ 3"))
  print ("5. Mixed Operators: 4 * (3 + 2) % 7 - 1 --> " ++ (parse "4 * (3 + 2) % 7 - 1"))
  print ("== Invalid Expressions ==")
  print ("1. Unmatched Parentheses: 2 * (4 + 3 - 1 --> " ++ (parse "2 * (4 + 3 - 1"))
  print ("2. Operators Without Operands: * 5 + 2 --> " ++ (parse "* 5 + 2"))
  -- print ("3. Incorrect Operator Usage: 4 / 0 --> " ++ (parse "4 / 0"))
  print ("4. Missing Operator: 5 (2 + 3) --> " ++ (parse "5 (2 + 3)"))
  print ("5. Invalid Characters: 7 & 3 --> " ++ (parse "7 & 3"))
  print ("6. Mismatched Parentheses: (((3 + 4) - 2) + (1) --> " ++ (parse "(((3 + 4) - 2) + (1)"))
  print ("7. Invalid Operator Usage: ((5 + 2) / (3 * 0)) --> " ++ (parse "((5 + 2) / (3 * 0))"))
  print ("8. Invalid Operator Sequence: ((2 -) 1 + 3) --> " ++ (parse "((2 -) 1 + 3)"))
  print ("9. Missing Operand: ((4 * 2) + ( - )) --> " ++ (parse "((4 * 2) + ( - ))"))
  print ("10. Invalid Characters ((7 * 3) ** 2) --> " ++ (parse "((7 * 3) ** 2)"))

---------------------------------------------------------------------------------------------
--   PARSE STRINGS
---------------------------------------------------------------------------------------------

-- split a string (s) into two parts surrounding a given character (c)
-- modified from https://stackoverflow.com/questions/11199475/string-parsing-in-haskell
split :: Char -> String -> [String]
split _ "" = []
split c s = [firstWord, rest]
  where
    firstWord = takeWhile (/= c) s -- before c
    rest = drop (length firstWord + 1) s -- after c

-- removes a character from string
-- taken from https://stackoverflow.com/questions/11199475/string-parsing-in-haskell
removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar ch (c : cs)
  | c == ch = removeChar ch cs
  | otherwise = c : removeChar ch cs

-- removes all spaces from string
clean :: String -> String
clean = removeChar ' '

---------------------------------------------------------------------------------------------
--   GROUPING CHECKING
---------------------------------------------------------------------------------------------

-- True if the string has parenthesis
hasGroup :: String -> Bool
hasGroup cs
  | ')' `elem` cs = True
  | '(' `elem` cs = True
  | otherwise = False

-- returns the number of open parenthesis
numOpen :: String -> Int
numOpen cs = length [c | c <- cs, c == '(']

-- returns the number of closed parenthesis
numClose :: String -> Int
numClose cs = length [c | c <- cs, c == ')']

-- remove all characters except open and closed parenthesis
getGroupSymbols :: String -> String
getGroupSymbols cs = [c | c <- cs, c == ')' || c == '(']

-- returns True if the string has proper grouping symbols
isBalanced :: String -> Bool
isBalanced s
  | not (hasGroup s) = True -- no parenthesis
  | (head gs /= ')') -- cannot start with closed parenthesis
      && (numOpen gs == numClose gs) -- must be an open for every closed parenthesis
      && (last gs /= '(') -- cannot end with open parenthesis
    =
      True
  | otherwise = False
  where
    gs = getGroupSymbols s -- get string of grouping symbols

---------------------------------------------------------------------------------------------
--   SYMBOL CHECKING
---------------------------------------------------------------------------------------------

-- String of numbers 0-9
getNumberChars :: [Char]
getNumberChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

-- String of allowed mathematical operators
getOperatorChars :: [Char]
getOperatorChars = ['+', '-', '*', '/', '%', '^']

-- String of allowed grouping symbols
getGroupingChars :: [Char]
getGroupingChars = ['(', ')']

-- String of all allowed characters for arithmetic
getValidChars :: [Char]
getValidChars = getNumberChars ++ getOperatorChars ++ getGroupingChars

-- True when the string has only allowed characters
hasValidSymbols :: String -> Bool
hasValidSymbols cs
  | null ([c | c <- cs, c `notElem` getValidChars]) = True -- list comprehension builds a list of invalid characters
  | otherwise = False

-- True if the first character in the String is not an operator
hasValidStart :: String -> Bool
hasValidStart cs = head cs `notElem` getOperatorChars

-- True if the last character in the String is not an operator
hasValidEnd :: String -> Bool
hasValidEnd cs = last cs `notElem` getOperatorChars

-- True if the string contains an operator
hasOperator :: String -> Bool
hasOperator cs = not (null [c | c <- cs, c `elem` getOperatorChars])

-- build a list of strings where each number is separated by an operator or grouping symbol
breakUp :: String -> [String]
breakUp cs
  | null cs = [] -- nothing to break
  | '%' `elem` cs = let (lhs, '%' : rhs) = break (== '%') cs in (breakUp lhs ++ ["%"] ++ breakUp rhs)
  | '*' `elem` cs = let (lhs, '*' : rhs) = break (== '*') cs in (breakUp lhs ++ ["*"] ++ breakUp rhs)
  | '/' `elem` cs = let (lhs, '/' : rhs) = break (== '/') cs in (breakUp lhs ++ ["/"] ++ breakUp rhs)
  | '+' `elem` cs = let (lhs, '+' : rhs) = break (== '+') cs in (breakUp lhs ++ ["+"] ++ breakUp rhs)
  | '-' `elem` cs = let (lhs, '-' : rhs) = break (== '-') cs in (breakUp lhs ++ ["-"] ++ breakUp rhs)
  | '(' `elem` cs = let (lhs, '(' : rhs) = break (== '(') cs in (breakUp lhs ++ ["("] ++ breakUp rhs)
  | ')' `elem` cs = let (lhs, ')' : rhs) = break (== ')') cs in (breakUp lhs ++ [")"] ++ breakUp rhs)
  | otherwise = [cs] -- just a number

---------------------------------------------------------------------------------------------
--   DEFINE OPERATORS
---------------------------------------------------------------------------------------------

-- Converts a string to an integer
toInt :: String -> Int
toInt s = read s :: Int

-- converts an integer to a string
toStr :: Int -> String
toStr n = show n

-- add x+y
addi :: Int -> Int -> Int
addi x y = x + y

-- subtract x-y
subi :: Int -> Int -> Int
subi x y = x - y

-- multiply x*y
mult :: Int -> Int -> Int
mult x y = x * y

-- divide x/y
divi :: Int -> Int -> Int
divi x y = x `div` y

-- modulo x%y
modu :: Int -> Int -> Int
modu x y = x `mod` y

-- compute the exponent x^y
expo :: Int -> Int -> Int
expo x y = x ^ y

-- perform one operation (f) with a character key (c) on two numbers in a string (s)
doOperation :: String -> Char -> (Int -> Int -> Int) -> String
doOperation s c f
  | null lhsRemain = parse (concat (op : rhsRemain)) -- nothing remaining on left hand side
  | null rhsRemain = parse (concat (lhsRemain ++ [op])) -- nothing remaining on right hand side
  | otherwise = op
  where
    [lhs, rhs] = split c s -- separate string into two halves on either side of the character
    lhsb = breakUp lhs -- separate the numbers and operators on the left hand string
    rhsb = breakUp rhs -- separate the numbers and operators on the right hand string
    op = toStr (f (toInt (last lhsb)) (toInt (head rhsb))) -- operate the numbers on the left and right sides
    rhsRemain = tail rhsb -- remove the first item
    lhsRemain = init lhsb -- remove the last item

-- solve problem with grouping symbols
doGroup :: String -> String
doGroup s
  | not (null lhs) && null rhs && not (last lhs `elem` getOperatorChars) = parse "Err" --
  | null lhs && not (null rhs) && not (head rhs `elem` getOperatorChars) = parse "Err" --
  | null lhs = parse (groupSoln ++ rhs)
  | null rhs = parse (lhs ++ groupSoln)
  | otherwise = groupSoln
  where
    [lhs, grhs] = split '(' s -- separate string into two halves on either side of the '(' character
    [g, rhs] = split ')' grhs -- separate string into two halves on either side of the ')' character
    groupSoln = parse g -- solve the problem between the grouping symbols

---------------------------------------------------------------------------------------------
--   SOLVE
---------------------------------------------------------------------------------------------

-- True when the strig is valid
isValid :: String -> Bool
isValid cs = isBalanced cs && hasValidSymbols cs && hasValidStart cs && hasValidEnd cs

-- throw an error if the string is invalid
errorCheck :: String -> String
errorCheck cs
  | null cs = "" -- empty string
  | isValid cs = cs
  | otherwise = "Err"

-- parse a string problem and calculate the solution
parse :: String -> String
parse cs
  | 'E' `elem` p = p -- Error
  | '(' `elem` p = doGroup p -- solve what is inside grouping symbols first
  | '^' `elem` p = doOperation p '^' expo -- next solve exponent
  | '*' `elem` p = doOperation p '*' mult -- multiplication
  | '/' `elem` p = doOperation p '/' divi -- division
  | '%' `elem` p = doOperation p '%' modu -- modulo
  | '+' `elem` p = doOperation p '+' addi -- addittion
  | '-' `elem` p = doOperation p '-' subi -- subtraction
  | otherwise = p -- just a number
  where
    p = errorCheck (clean cs) -- remove spaces from string and check for errors