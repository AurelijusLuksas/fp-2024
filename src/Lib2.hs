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
    parseCreateEmptyList,
    parseCreateList,
    parseFind,
    parseAdd,
    parseRemove,
    parseGet,
    parseDelete,
    parseGetList
) where

import Data.Char (isSpace, isDigit, isLetter)
import Data.List (isPrefixOf, find, intercalate)

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
    | CreateList IngredientList
    | CreateEmptyList Name
    | GetList Name
    | Delete Name
    | Find Ingredient
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

-- Update a state according to a query
stateTransition :: State -> Query -> Either String ([String], State)
stateTransition (State lists ings) (Create name qty unit) =
    let newIng = Ingredient name qty unit
    in Right (["Created ingredient"], State lists (newIng : ings))

stateTransition (State lists ings) (Add ingName listName) =
    let updatedLists = map (\(name, items) -> if name == listName then (name, addToList ingName items) else (name, items)) lists
        addToList ingName items =
            case find (\(Ingredient n _ _) -> n == ingName) ings of
                Just ing -> addIngredientToList ing items
                Nothing -> case lookup ingName lists of
                    Just sublist -> addSublistToList sublist items
                    Nothing -> items
        addIngredientToList ing [] = []
        addIngredientToList ing (IngredientList n is sublists : rest) =
            IngredientList n (ing : is) sublists : rest
        addSublistToList sublist [] = []
        addSublistToList sublist (IngredientList n is sublists : rest) =
            IngredientList n is (sublist ++ sublists) : rest
    in if any (\(name, items) -> name == listName) updatedLists
       then Right (["Added ingredient to list"], State updatedLists ings)
       else Left "Ingredient or list not found"

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

stateTransition (State lists ings) (CreateList (IngredientList name items sublists)) =
    if any (\(n, _) -> n == name) lists
    then Left "List already exists"
    else
        let newList = (name, [IngredientList name items sublists])
            newLists = newList : lists
        in Right (["Created ingredient list"], State newLists ings)

stateTransition (State lists ings) (CreateEmptyList name) =
    if any (\(n, _) -> n == name) lists
    then Left "List already exists"
    else
        let newList = (name, [IngredientList name [] []])
            newLists = newList : lists
        in Right (["Created empty ingredient list"], State newLists ings)

stateTransition (State lists ings) (Find ingredient) =
    let foundLists = findIngredientInLists ingredient lists
    in if null foundLists
       then Left "Ingredient not found in any list"
       else Right (["Found in lists: " ++ intercalate ", " (map formatName foundLists)], State lists ings)
    where
        findIngredientInLists :: Ingredient -> [(Name, [IngredientList])] -> [Name]
        findIngredientInLists _ [] = []
        findIngredientInLists ing ((name, items):rest) =
            if any (ingredientInList ing) items
            then name : findIngredientInLists ing rest
            else findIngredientInLists ing rest

        ingredientInList :: Ingredient -> IngredientList -> Bool
        ingredientInList ing (IngredientList _ is sublists) =
            ing `elem` is || any (ingredientInList ing) sublists

        formatName :: Name -> String
        formatName (WordName n) = n
        formatName (NumberName n) = show n
        formatName (StringName n) = n

stateTransition state@(State lists ings) (GetList name) = 
    case findList name lists of
        Just list -> 
            let output = formatList name list
            in Right ([output, "Found list"], state)
        Nothing -> 
            Left "List not found"
    where
        findList :: Name -> [(Name, [IngredientList])] -> Maybe [IngredientList]
        findList _ [] = Nothing
        findList name ((n, items):rest)
            | name == n = Just items
            | otherwise = case findListInItems name items of
                Just list -> Just list
                Nothing -> findList name rest

        findListInItems :: Name -> [IngredientList] -> Maybe [IngredientList]
        findListInItems _ [] = Nothing
        findListInItems name (IngredientList n is sublists : rest)
            | name == n = Just [IngredientList n is sublists]
            | otherwise = case findListInItems name sublists of
                Just list -> Just list
                Nothing -> findListInItems name rest
        
        formatList :: Name -> [IngredientList] -> String
        formatList name lists = formatName name ++ ": {\n" ++ unlines (map formatIngredientList lists) ++ "}"

        formatIngredientList :: IngredientList -> String
        formatIngredientList (IngredientList n is sublists) = 
            unlines (map formatIngredient is) ++ 
            concatMap (\sub -> 
                        "\t" ++ formatName (getName sub) ++ ": {\n" ++ 
                        indent (formatIngredientList sub) ++ "\t}\n") sublists
            where
                getName :: IngredientList -> Name
                getName (IngredientList name _ _) = name


        formatIngredient :: Ingredient -> String
        formatIngredient (Ingredient n (Quantity q) unit) = 
            "\t" ++ formatName n ++ ": " ++ show q ++ " " ++ show unit

        formatName :: Name -> String
        formatName (WordName n) = n  -- Adjust this line based on the actual structure of Name

        indent :: String -> String
        indent = unlines . map ("\t" ++) . lines
    

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
    in case parseNumber trimmedInput of
        Right (number, rest) -> Right (NumberName number, rest)
        Left _ -> case parseWord trimmedInput of
            Right (word, rest) -> Right (WordName word, rest)
            Left err -> error err


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
parseIngredientList = and5' (\name _ _ (ingredients, nestedLists) _ -> IngredientList name ingredients nestedLists) parseName (parseChar ':') (parseChar '{') parseMoreIngredientsAndLists (parseChar '}')

parseMoreIngredientsAndLists :: Parser ([Ingredient], [IngredientList])
parseMoreIngredientsAndLists input = 
    case parseIngredient input of
        Right (ing, rest) -> 
            case parseChar ',' rest of
                Right (_, rest') -> 
                    case parseMoreIngredientsAndLists rest' of
                        Right ((ings, lists), rest'') -> 
                            Right ((ing : ings, lists), rest'')
                        Left _ -> 
                            Right (([ing], []), rest')
                Left _ -> 
                    Right (([ing], []), rest)
        Left _ -> 
            case parseIngredientList input of
                Right (list, rest) -> 
                    case parseChar ',' rest of
                        Right (_, rest') -> 
                            case parseMoreIngredientsAndLists rest' of
                                Right ((ings, lists), rest'') -> 
                                    Right ((ings, list : lists), rest'')
                                Left _ -> 
                                    Right (([], [list]), rest')
                        Left _ -> 
                            Right (([], [list]), rest)
                Left _ -> 
                    Right (([], []), input)

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
                    Left err4 -> case parseCreateEmptyList input of
                        Right (query, _) -> Right query
                        Left err5 -> case parseDelete input of
                            Right (query, _) -> Right query
                            Left err6 -> case parseGetList input of
                                Right (query, _) -> Right query
                                Left err7 -> (case parseCreateList input of
                                    Right (query, _) -> Right query
                                    Left err8 -> (case parseFind input of
                                        Right (query, _) -> Right query
                                        Left err9 -> Left $ " Parse errors: " ++ err1 ++ ", " ++ err2 ++ ", "
                                            ++ err3 ++ ", " ++ err4 ++ ", " ++ err5 ++ ", " ++ err6 ++ ", "
                                            ++ err7 ++ ", " ++ err8 ++ ", " ++ err9))

-- <create> ::= "create (" <name> ", " <quantity> ", " <unit> ")"
parseCreate :: Parser Query
parseCreate = and7' create (string "create(") parseName (parseChar ',') parseQuantity (parseChar ',') parseUnit (parseChar ')')
    where create _ name _ qty _ unit _ = Create name qty unit

-- <find> ::= "find (" <ingredient> ")"
parseFind :: Parser Query
parseFind input = case and3' (\_ ingredient _ -> Find ingredient) (string "find(") parseIngredient (parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

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

-- <create_list> ::= "create_list (" <ingredients_list> ")"
parseCreateList :: Parser Query
parseCreateList input = case and3' (\_ ingredientList _ -> CreateList ingredientList) (string "create_list(") parseIngredientList (parseChar ')') input of
    Right (query, rest) -> Right (query, rest)
    Left err -> Left err

-- <create_empty_list> ::= "create_list (" <name> ")"
parseCreateEmptyList :: Parser Query
parseCreateEmptyList input = case and2' (\_ name -> CreateEmptyList name) (string "create_empty_list(") (parseName <* parseChar ')') input of
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