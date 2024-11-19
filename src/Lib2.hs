{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
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
    parseDelete,
    parseGetList
) where

import Data.Char (isSpace, isDigit, isLetter)
import Data.List (isPrefixOf, find)
import Debug.Trace
import GHC.Base (TrName)

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
    | Add Name Name
    | Remove Name Name
    | Get Name
    | CreateList Name
    | GetList Name
    | Delete Name
    | DeleteList Name
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

formatIngredient :: Ingredient -> String
formatIngredient (Ingredient (WordName name) (Quantity qty) unit) = name ++ ": " ++ show qty ++ " " ++ show unit
formatIngredient (Ingredient (NumberName num) (Quantity qty) unit) = show num ++ ": " ++ show qty ++ " " ++ show unit
formatIngredient (Ingredient (StringName str) (Quantity qty) unit) = str ++ ": " ++ show qty ++ " " ++ show unit

-- Main function to format an ingredient list
formatIngredientList :: Name -> [IngredientList] -> String
formatIngredientList name lists = formatName name ++ ": {" ++ formatLists lists ++ "}"

-- Function to format the name of an ingredient or list
formatName :: Name -> String
formatName (WordName name) = name
formatName (NumberName num) = show num
formatName (StringName str) = str

-- Function to format multiple ingredient lists at the top level
formatLists :: [IngredientList] -> String
formatLists [] = ""
formatLists (IngredientList name is sublists : xs) = 
    formatIngredients is 
    ++ (if not (null is) && not (null sublists) then ", " else "")
    ++ formatFlatSublists sublists 
    ++ (if null xs then "" else ", " ++ formatLists xs)

-- Function to format ingredients with commas between them
formatIngredients :: [Ingredient] -> String
formatIngredients [] = ""
formatIngredients [x] = formatIngredient x
formatIngredients (x:xs) = formatIngredient x ++ ", " ++ formatIngredients xs

-- Function to format sublists without nesting or duplication
formatFlatSublists :: [IngredientList] -> String
formatFlatSublists [] = ""
formatFlatSublists (IngredientList name is [] : xs) = 
    formatName name ++ ": {" ++ formatIngredients is ++ "}" 
    ++ (if null xs then "" else ", " ++ formatFlatSublists xs)
formatFlatSublists (IngredientList name is sublists : xs) = 
    formatName name ++ ": {" ++ formatIngredients is 
    ++ (if not (null sublists) then ", " ++ formatFlatSublists sublists else "") ++ "}" 
    ++ (if null xs then "" else ", " ++ formatFlatSublists xs)

-- Update a state according to a query
stateTransition :: State -> Query -> Either String ([String], State)
stateTransition (State lists ings) (Create name qty unit) =
    let newIng = Ingredient name qty unit
    in Right (["Created ingredient"], State lists (newIng : ings))

stateTransition (State lists ings) (Add ingName listName) =
    case find (\(Ingredient n _ _) -> n == ingName) ings of
        Just ing ->
            let updatedLists = map (\(name, items) -> if name == listName then (name, addIngredientToList ing items) else (name, items)) lists
                addIngredientToList ing (IngredientList n is sublists : rest)
                    | n == listName = IngredientList n (ing : is) sublists : rest
                    | otherwise = IngredientList n is (addIngredientToList ing sublists) : addIngredientToList ing rest
                addIngredientToList _ [] = []
            in if any (\(name, items) -> name == listName || not (null (addIngredientToList ing items))) updatedLists
               then Right (["Added ingredient to list"], State updatedLists ings)
               else Left "Ingredient or list not found"
        Nothing ->
            case find (\(n, items) -> n == ingName) lists of
                Just (sublistName, sublistItems) ->
                    let updatedLists = map (\(name, items) -> if name == listName then (name, addListToList sublistName sublistItems items) else (name, items)) lists
                        addListToList sublistName sublistItems (IngredientList n is sublists : rest)
                            | n == listName = IngredientList n is (IngredientList sublistName is sublistItems : sublists) : rest
                            | otherwise = IngredientList n is (addListToList sublistName sublistItems sublists) : addListToList sublistName sublistItems rest
                        addListToList _ _ [] = []
                    in if any (\(name, items) -> name == listName || not (null (addListToList sublistName sublistItems items))) updatedLists
                       then Right (["Added list to list"], State updatedLists ings)
                       else Left "Ingredient or list not found"
                Nothing -> Left "Ingredient or list not found"

stateTransition (State lists ings) (Remove ingName listName) =
    let updatedLists = map (\(name, items) -> if name == listName then (name, removeIngredientFromList ingName items) else (name, items)) lists
        removeIngredientFromList _ [] = []
        removeIngredientFromList ingName (IngredientList n is sublists : rest)
            | n == listName = IngredientList n (filter (\(Ingredient n _ _) -> n /= ingName) is) sublists : rest
            | otherwise = IngredientList n is (removeIngredientFromList ingName sublists) : removeIngredientFromList ingName rest
    in if any (\(name, items) -> name == listName && not (null (filter (\(Ingredient n _ _) -> n == ingName) (concatMap (\(IngredientList _ is _) -> is) items)))) updatedLists
       then Right (["Removed ingredient"], State updatedLists ings)
       else if any (\(name, items) -> not (null (removeIngredientFromList ingName items))) updatedLists
            then Right (["Removed ingredient"], State updatedLists ings)
            else Left "Ingredient or list not found"

stateTransition (State lists ings) (Get name) =
    case find (\(Ingredient n _ _) -> n == name) ings of
        Just (Ingredient (WordName actualName) (Quantity qty) unit) -> Right ([actualName ++ ": " ++ show qty ++ " " ++ show unit], State lists ings)
        Nothing -> Left "Ingredient not found"

stateTransition (State lists ings) (CreateList name) =
    if any (\(n, _) -> n == name) lists
    then Left "List already exists"
    else
        let newList = (name, [IngredientList name [] []])
            newLists = newList : lists
        in Right (["Created ingredient list"], State newLists ings)

stateTransition (State lists ings) (GetList name) =
    case find (\(n, items) -> n == name) lists of
        Just (n, items) -> 
            let formattedList = formatIngredientList n items
            in Right ([formattedList], State lists ings)
        Nothing -> Left "List not found"

stateTransition (State lists ings) (Delete name) =
    case find (\(Ingredient n _ _) -> n == name) ings of
        Just _ ->
            let remainingIngs = filter (\(Ingredient n _ _) -> n /= name) ings
            in Right (["Deleted ingredient"], State lists remainingIngs)
        Nothing ->
            let updatedLists = map (\(listName, items) -> (listName, deleteFromList name items)) lists
                deleteFromList _ [] = []
                deleteFromList name (IngredientList n is sublists : rest)
                    | n == name = rest
                    | otherwise = IngredientList n is (deleteFromList name sublists) : deleteFromList name rest
            in if any (\(listName, items) -> listName == name || not (null (deleteFromList name items))) lists
               then Right (["Deleted list"], State (filter (\(listName, items) -> listName /= name && not (null items)) updatedLists) ings)
               else Left "Ingredient or list not found"

-- Helper functions for parsing
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f a b input =
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (f v1 v2, r2)
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
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

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

string :: String -> Parser String
string str input
    | str `isPrefixOf` input = Right (str, drop (length str) input)
    | otherwise = Left $ "Expected '" ++ str ++ "', but got '" ++ take (length str) input ++ "'"

parseChar :: Char -> Parser Char
parseChar c input = 
    let skipWhitespace = dropWhile isSpace input
    in case skipWhitespace of
        [] -> Left ("Cannot find " ++ [c] ++ " in an empty input")
        (h:t) -> if c == h then Right (c, t) else Left (c : " is not found in " ++ [h])

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
parseIngredient = and4' (\name _ qty unit -> Ingredient name qty unit) parseName (parseChar ':') parseQuantity (parseChar ' ' *> parseUnit)

parseIngredientList :: Parser IngredientList
parseIngredientList = and5' (\name _ _ ingredients _ -> IngredientList name ingredients []) parseName (parseChar ':') (parseChar '{') parseIngredients (parseChar '}')

parseIngredients :: Parser [Ingredient]
parseIngredients input = trace ("parseIngredients input: " ++ show input) $
    case parseIngredient input of
        Right (ing, rest) -> trace ("Parsed ingredient: " ++ show ing ++ ", rest: " ++ show rest) $
            case parseChar ',' rest of
                Right (_, rest') -> trace ("Found comma, rest': " ++ show rest') $
                    case parseIngredients rest' of
                        Right (ings, rest'') -> trace ("Parsed more ingredients: " ++ show ings ++ ", rest'': " ++ show rest'') $
                            Right (ing : ings, rest'')
                        Left _ -> trace ("Failed to parse more ingredients, returning: " ++ show [ing] ++ ", rest': " ++ show rest') $
                            Right ([ing], rest')
                Left _ -> trace ("No comma found, returning: " ++ show [ing] ++ ", rest: " ++ show rest) $
                    Right ([ing], rest)
        Left _ -> trace ("Failed to parse ingredient, returning empty list, input: " ++ show input) $
            Right ([], input)


parseQuery :: String -> Either String Query
parseQuery input = 
    case parseCreate input of
        Right (query, _) -> Right query
        Left err1 -> case parseAdd input of
            Right (query, _) -> Right query
            Left err2 -> case parseRemove input of
                Right (query, _) -> Right query
                Left err3 -> case parseGet input of
                    Right (query, _) -> Right query
                    Left err4 -> case parseCreateList input of
                        Right (query, _) -> Right query
                        Left err5 -> case parseDelete input of
                            Right (query, _) -> Right query
                            Left err6 -> case parseGetList input of
                                Right (query, _) -> Right query
                                Left err7 -> Left $ " Parse errors: " ++ err1 ++ ", " ++ err2 ++ ", "
                                    ++ err3 ++ ", " ++ err4 ++ ", " ++ err5 ++ ", " ++ err6 ++ ", "
                                    ++ err7

-- <create> ::= "create (" <name> ", " <quantity> ", " <unit> ")"
parseCreate :: Parser Query
parseCreate = and7' create (string "create(") parseName (parseChar ',') parseQuantity (parseChar ',') parseUnit (parseChar ')')
    where create _ name _ qty _ unit _ = Create name qty unit

-- <add> ::= "add (" <name> ", " <list_name> ")"
parseAdd :: Parser Query
parseAdd = and5' add (string "add(") parseName (parseChar ',') parseName (parseChar ')')
    where add _ name _ listName _ = Add name listName

-- <remove> ::= "remove (" <name> ", " <list_name> ")"
parseRemove :: Parser Query
parseRemove = and5' remove (string "remove(") parseName (parseChar ',') parseName (parseChar ')')
    where remove _ name _ listName _ = Remove name listName

-- <get> ::= "get (" <name> ")"
parseGet :: Parser Query
parseGet input = case and2' (\_ name -> Get name) (string "get(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

-- <create_list> ::= "create_list (" <name> ")"
parseCreateList :: Parser Query
parseCreateList input = case and2' (\_ name -> CreateList name) (string "create_list(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

-- <get_list> ::= "get_list (" <name> ")"
parseGetList :: Parser Query
parseGetList input = case and2' (\_ name -> GetList name) (string "get_list(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

-- <delete> ::= "delete (" <name> ")"
parseDelete :: Parser Query
parseDelete input = case and2' (\_ name -> Delete name) (string "delete(") (parseName <* parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err