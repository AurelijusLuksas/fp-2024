{-# LANGUAGE InstanceSigs #-}
module Lib2 (
    Query(..),
    State(..),
    Name(..),
    Quantity(..),
    Unit(..),
    Ingredient(..),
    IngredientList(..),
    parseQuery,
    emptyState,
    stateTransition,
    parseName,
    parseQuantity,
    parseUnit,
    parseIngredient,
    parseIngredientList,
    parseCreate,
    parseCreateList,
    parseAdd,
    parseRemove,
    parseGet,
    parseSelect,
    parseUpdate,
    parseDelete
) where

import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.List (isPrefixOf, find, delete)
import Debug.Trace

-- Define the Name data type
data Name = NumberName Int | WordName String | StringName String
    deriving (Show, Eq, Ord)

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

-- Define the State data type
data State = State {
    ingredientLists :: [(Name, [IngredientList])],
    ingredients :: [Ingredient]
} deriving (Show, Eq)

-- Create an initial program's state
emptyState :: State
emptyState = State {
    ingredientLists = [],
    ingredients = []
}

-- Update a state according to a query
stateTransition :: State -> Query -> Either String ([String], State)
stateTransition (State lists ings) (Create name qty unit) =
    let newIng = Ingredient name qty unit
    in Right (["Created ingredient"], State lists (newIng : ings))

stateTransition (State lists ings) (Add name) =
    case find (\(Ingredient n _ _) -> n == name) ings of
        Just ing -> Right (["Added ingredient"], State lists (ing : ings))
        Nothing -> Left "Ingredient not found"

stateTransition (State lists ings) (Remove name) =
    let newIngs = filter (\(Ingredient n _ _) -> n /= name) ings
    in Right (["Removed ingredient"], State lists newIngs)

stateTransition (State lists ings) (Get name) =
    case find (\(Ingredient n _ _) -> n == name) ings of
        Just _ -> Right (["Got ingredient"], State lists ings)
        Nothing -> Left "Ingredient not found"

stateTransition (State lists ings) (Select ingredient) =
    Right (["Selected ingredient"], State lists (ingredient : ings))

stateTransition (State lists ings) (SelectName name) =
    case find (\(Ingredient n _ _) -> n == name) ings of
        Just _ -> Right (["Selected name"], State lists ings)
        Nothing -> Left "Name not found"

stateTransition (State lists ings) (Update name1 name2 qty unit) =
    let newIngs = map (\(Ingredient n q u) -> if n == name1 then Ingredient name2 qty unit else Ingredient n q u) ings
    in Right (["Updated ingredient"], State lists newIngs)

stateTransition (State lists ings) (CreateList name) =
    let newList = (name, [])
        newLists = newList : lists
    in Right (["Created list"], State newLists ings)

stateTransition (State lists ings) (Delete name) =
    let newLists = filter (\(n, _) -> n /= name) lists
    in Right (["Deleted list"], State newLists ings)

-- Helper functions for parsing
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f a b input =
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (f v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f a b c input =
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (f v1 v2 v3, r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f a b c d input =
    case a input of
        Right (v1, r1) -> 
            case b r1 of
                Right (v2, r2) -> 
                    case c r2 of
                        Right (v3, r3) -> 
                            case d r3 of
                                Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                                Left e4 -> Left $ "Error in fourth parser: " ++ e4
                        Left e3 -> Left $ "Error in third parser: " ++ e3
                Left e2 -> Left $ "Error in second parser: " ++ e2
        Left e1 -> Left $ "Error in first parser: " ++ e1

and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5' f a b c d e input =
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) ->
                                    case e r4 of
                                        Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                                        Left e5 -> Left e5
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

and7' :: (a -> b -> c -> d -> e -> f -> g -> h)
      -> Parser a
      -> Parser b
      -> Parser c
      -> Parser d
      -> Parser e
      -> Parser f
      -> Parser g
      -> Parser h
and7' g a b c d e f g' input =
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) ->
                                    case e r4 of
                                        Right (v5, r5) ->
                                            case f r5 of
                                                Right (v6, r6) ->
                                                    case g' r6 of
                                                        Right (v7, r7) -> Right (g v1 v2 v3 v4 v5 v6 v7, r7)
                                                        Left e7 -> Left e7
                                                Left e6 -> Left e6
                                        Left e5 -> Left e5
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

or2 :: Parser a -> Parser a -> Parser a
or2 a b input =
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)

string :: String -> Parser String
string str input
    | str `isPrefixOf` input = Right (str, drop (length str) input)
    | otherwise = Left $ "Expected '" ++ str ++ "', but got '" ++ take (length str) input ++ "'"

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
    let skipWhitespace = dropWhile (== ' ')
        trimmedInput = skipWhitespace input
    in case parseWord trimmedInput of
        Right (word, rest) -> Right (WordName word, rest)
        Left _ -> case parseNumber input of
            Right (num, rest) -> Right (NumberName num, rest)
            Left err -> Left err

parseQuantity :: Parser Quantity
parseQuantity input = 
    let skipWhitespace = dropWhile (== ' ')
        trimmedInput = skipWhitespace input
    in case parseNumber trimmedInput of
        Right (n, rest) -> Right (Quantity n, skipWhitespace rest)
        Left err -> Left err

parseUnit :: Parser Unit
parseUnit input = 
    let skipWhitespace = dropWhile (== ' ')
        trimmedInput = skipWhitespace input
    in case span isLetter trimmedInput of
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
            Right (ings, rest') -> Right (ing : ings, rest')
            Left _ -> Right ([ing], rest)
        Left _ -> Right ([], input)

parseQuery :: String -> Either String Query
parseQuery input = 
    let debugInput = "Input received: " ++ input
    in trace debugInput $ case parseCreate input of
        Right (query, _) -> Right query
        Left err1 -> case parseAdd input of
            Right (query, _) -> Right query
            Left err2 -> case parseRemove input of
                Right (query, _) -> Right query
                Left err3 -> case parseGet input of
                    Right (query, _) -> Right query
                    Left err4 -> case parseSelect input of
                        Right (query, _) -> Right query
                        Left err5 -> case parseUpdate input of
                            Right (query, _) -> Right query
                            Left err6 -> case parseCreateList input of
                                Right (query, _) -> Right query
                                Left err7 -> case parseDelete input of
                                    Right (query, _) -> Right query
                                    Left err8 -> Left $ debugInput ++ " | Parse errors: " ++ err1 ++ ", " ++ err2 ++ ", " ++ err3 ++ ", " ++ err4 ++ ", " ++ err5 ++ ", " ++ err6 ++ ", " ++ err7 ++ ", " ++ err8

parseCreate :: Parser Query
parseCreate = and7' create (string "create(") parseName (parseChar ',') parseQuantity (parseChar ',') parseUnit (parseChar ')')
    where create _ name _ qty _ unit _ = Create name qty unit

parseAdd :: Parser Query
parseAdd input = case and2' (\_ name -> Add name) (string "add(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

parseRemove :: Parser Query
parseRemove input = case and2' (\_ name -> Remove name) (string "remove(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

parseGet :: Parser Query
parseGet input = case and2' (\_ name -> Get name) (string "get(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

parseSelect :: Parser Query
parseSelect input = case and2' (\_ ing -> Select ing) (string "select(") (parseIngredient <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

parseUpdate :: Parser Query
parseUpdate input = case and5' (\_ name1 name2 qty unit -> Update name1 name2 qty unit) (string "update(") parseName (parseChar ',' *> parseName) (parseChar ',' *> parseQuantity) (parseChar ',' *> parseUnit <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

parseCreateList :: Parser Query
parseCreateList input = case and2' (\_ name -> CreateList name) (string "create_list(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

parseDelete :: Parser Query
parseDelete input = case and2' (\_ name -> Delete name) (string "delete(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err