{-# LANGUAGE InstanceSigs #-}
module Lib2 (
    Query(..),
    State(..),
    Name(..),
    parseQuery,
    emptyState,
    stateTransition
) where

import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.List (isPrefixOf)

-- Define the Name data type
data Name = NumberName Int | WordName String | StringName String
    deriving (Show, Eq)

-- Define the Quantity and Unit data types
newtype Quantity = Quantity Int
    deriving (Show, Eq)

data Unit = Cup | Cups | Tbsp | Tsp | Oz | Lb | G | Kg | Ml | L | Pinch | Cloves | Full | Half
    deriving (Show, Eq)

-- Define the Ingredient data type
data Ingredient = Ingredient Name Quantity Unit
    deriving (Show, Eq)

-- Define the IngredientList data type
data IngredientList = IngredientList Name [Ingredient] [IngredientList]
    deriving (Show, Eq)

-- Define the Query data type
data Query
    = Create Name Quantity Unit
    | Add Name
    | Remove Name
    | Get Name
    | Select Ingredient
    | SelectName Name
    | Update Name Name Quantity Unit
    | CreateList Name
    | Delete Name
    deriving (Show, Eq)

-- Define the parser type
type Parser a = String -> Either String (a, String)

-- Helper functions for parsing
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f a b input =
    case a input of
        Right (v1, r1) -> case b r1 of
            Right (v2, r2) -> Right (f v1 v2, r2)
            Left e2 -> Left e2
        Left e1 -> Left e1

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f pa pb pc input =
    case pa input of
        Right (a, rest1) -> case pb rest1 of
            Right (b, rest2) -> case pc rest2 of
                Right (c, rest3) -> Right (f a b c, rest3)
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f pa pb pc pd input =
    case pa input of
        Right (a, rest1) -> case pb rest1 of
            Right (b, rest2) -> case pc rest2 of
                Right (c, rest3) -> case pd rest3 of
                    Right (d, rest4) -> Right (f a b c d, rest4)
                    Left err -> Left err
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err

or2 :: Parser a -> Parser a -> Parser a
or2 a b input =
    case a input of
        Right r1 -> Right r1
        Left e1 -> case b input of
            Right r2 -> Right r2
            Left e2 -> Left (e1 ++ ", " ++ e2)

string :: String -> Parser String
string str input
    | str `isPrefixOf` input = Right (str, drop (length str) input)
    | otherwise = Left $ "Expected '" ++ str ++ "'"

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c (h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ [h])

parseWord :: Parser String
parseWord input =
    let letters = takeWhile isLetter input
        rest = drop (length letters) input
    in if not (null letters)
        then Right (letters, rest)
        else Left (input ++ " does not start with a letter")

parseNumber :: Parser Int
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let digits = takeWhile isDigit str
        rest = drop (length digits) str
    in if null digits
        then Left "not a number"
        else Right (read digits, rest)

parseName :: Parser Name
parseName input =
    case parseWord input of
        Right (word, rest) -> Right (WordName word, rest)
        Left _ -> case parseNumber input of
            Right (num, rest) -> Right (NumberName num, rest)
            Left err -> Left err

parseQuantity :: Parser Quantity
parseQuantity input = fmap (\(n, rest) -> (Quantity n, rest)) (parseNumber input)

parseUnit :: Parser Unit
parseUnit input = case span isLetter input of
    ("cup", rest) -> Right (Cup, rest)
    ("cups", rest) -> Right (Cups, rest)
    ("tbsp", rest) -> Right (Tbsp, rest)
    ("tsp", rest) -> Right (Tsp, rest)
    ("oz", rest) -> Right (Oz, rest)
    ("lb", rest) -> Right (Lb, rest)
    ("g", rest) -> Right (G, rest)
    ("kg", rest) -> Right (Kg, rest)
    ("ml", rest) -> Right (Ml, rest)
    ("l", rest) -> Right (L, rest)
    ("pinch", rest) -> Right (Pinch, rest)
    ("cloves", rest) -> Right (Cloves, rest)
    ("full", rest) -> Right (Full, rest)
    ("half", rest) -> Right (Half, rest)
    _ -> Left "Invalid unit"

parseIngredient :: Parser Ingredient
parseIngredient = and3' Ingredient parseName (parseChar ':' *> parseQuantity) parseUnit

parseIngredientList :: Parser IngredientList
parseIngredientList = and3' (\name _ ingredients -> IngredientList name ingredients []) parseName (parseChar ':') (parseChar '{' *> parseIngredients <* parseChar '}')

parseIngredients :: Parser [Ingredient]
parseIngredients input =
    case parseIngredient input of
        Right (ing, rest) -> case parseIngredients rest of
            Right (ings, rest') -> Right (ing:ings, rest')
            Left _ -> Right ([ing], rest)
        Left _ -> Right ([], input)

parseQuery :: Parser Query
parseQuery = parseCreate `or2` parseAdd `or2` parseRemove `or2` parseGet `or2` parseSelect `or2` parseUpdate `or2` parseCreateList `or2` parseDelete

parseCreate :: Parser Query
parseCreate = and4' (\_ name qty unit -> Create name qty unit) (string "create (") parseName (parseChar ',') parseQuantity parseUnit

parseAdd :: Parser Query
parseAdd = and2' (\_ name -> Add name) (string "add (") parseName

parseRemove :: Parser Query
parseRemove = and2' (\_ name -> Remove name) (string "remove (") parseName

parseGet :: Parser Query
parseGet = and2' (\_ name -> Get name) (string "get (") parseName

parseSelect :: Parser Query
parseSelect = and2' (\_ ing -> Select ing) (string "select (") parseIngredient

parseUpdate :: Parser Query
parseUpdate = and4' (\_ name1 name2 qty unit -> Update name1 name2 qty unit) (string "update (") parseName (parseChar ',') parseName parseQuantity parseUnit

parseCreateList :: Parser Query
parseCreateList = and2' (\_ name -> CreateList name) (string "create_list (") parseName

parseDelete :: Parser Query
parseDelete = and2' (\_ name -> Delete name) (string "delete (") parseName

-- | The instances are needed basically for tests
-- instance Eq Query where
--   (==) _ _= False

-- instance Show Query where
--   show _ = ""

-- | Parses user's input.
-- The function must have tests.
-- parseQuery :: String -> Either String Query
-- parseQuery _ = Left "Not implemented 2"

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"