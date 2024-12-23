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
    parseGetList,
    Parser
) where

import qualified Control.Monad.Trans.State.Strict as S (State, get, put, runState)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace, isDigit, isLetter)
import Data.List (isPrefixOf, find, intercalate)

type Parser a = ExceptT String (S.State String) a

-- Define the Name data type
data Name = NumberName Int | WordName String | StringName String
    deriving (Eq, Ord)

-- Define the Quantity and Unit data types
newtype Quantity = Quantity Int
    deriving ( Eq)

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
    | AddList Name Name
    | Remove Name Name
    | Get Name
    | CreateList IngredientList
    | CreateEmptyList Name
    | GetList Name
    | Delete Name
    | Find Ingredient
    deriving (Show, Eq)

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
    let updatedLists = map (updateList ingName listName) lists
        updateList ingName listName (name, items) =
            if name == listName
            then (name, map (addToList ingName) items)
            else (name, map (updateSubList ingName listName) items)
        addToList ingName (IngredientList n is sublists) =
            IngredientList n (case find (\(Ingredient n _ _) -> n == ingName) ings of
                Just ing -> ing : is
                Nothing -> is) sublists
        updateSubList ingName listName (IngredientList n is sublists) =
            if n == listName
            then IngredientList n (case find (\(Ingredient n _ _) -> n == ingName) ings of
                Just ing -> ing : is
                Nothing -> is) sublists
            else IngredientList n is (map (updateSubList ingName listName) sublists)
    in Right (["Added ingredient to list"], State updatedLists ings)

stateTransition (State lists ings) (AddList listName parentListName) =
    let updatedLists = map (updateList listName parentListName) lists
        updateList listName parentListName (name, items) =
            if name == parentListName
            then (name, addToList listName items)
            else (name, map (updateSubList listName parentListName) items)
        addToList listName items =
            case find (\(IngredientList n _ _) -> n == listName) (concatMap snd lists) of
                Just sublist -> addSublistToList sublist items
                Nothing -> items
        addSublistToList sublist [] = []
        addSublistToList sublist (IngredientList n is sublists : rest) =
            IngredientList n is (sublist : sublists) : rest
        updateSubList listName parentListName (IngredientList n is sublists) =
            if n == parentListName
            then IngredientList n is (addToList listName sublists)
            else IngredientList n is (map (updateSubList listName parentListName) sublists)
    in Right (["Added ingredient list to list"], State updatedLists ings)

stateTransition (State lists ings) (GetList listName) =
    case find (\(name, _) -> name == listName) lists of
        Just (_, ingredientLists) -> Right (["Retrieved list:\n" ++ unlines (map formatIngredientList ingredientLists)], State lists ings)
        Nothing -> Left "List not found"

stateTransition (State lists ings) (Remove ingName listName) =
    let updatedLists = map (updateList ingName listName) lists
        updateList ingName listName (name, items) =
            if name == listName
            then (name, removeFromList ingName items)
            else (name, map (updateSubList ingName listName) items)
        removeFromList ingName [] = []
        removeFromList ingName (IngredientList n is sublists : rest) =
            IngredientList n (filter (\(Ingredient n _ _) -> n /= ingName) is) (map (updateSubList ingName listName) sublists) : removeFromList ingName rest
        updateSubList ingName listName (IngredientList n is sublists) =
            if n == listName
            then IngredientList n (filter (\(Ingredient n _ _) -> n /= ingName) is) sublists
            else IngredientList n is (map (updateSubList ingName listName) sublists)
    in if any (\(name, items) -> name == listName && not (null (filter (\(Ingredient n _ _) -> n == ingName) (concatMap (\(IngredientList _ is _) -> is) items)))) updatedLists
       then Right (["Removed ingredient"], State updatedLists ings)
       else if any (\(name, items) -> not (null (removeFromList ingName items))) updatedLists
            then Right (["Removed ingredient"], State updatedLists ings)
            else Left "Ingredient or list not found"

stateTransition (State lists ings) (Get name) =
    case find (\(Ingredient n _ _) -> n == name) ings of
        Just (Ingredient (WordName actualName) (Quantity qty) unit) -> Right ([actualName ++ ": " ++ show qty ++ " " ++ show unit], State lists ings)
        Nothing -> Left "Ingredient not found"

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

formatIngredientList :: IngredientList -> String
formatIngredientList (IngredientList name ingredients sublists) =
    formatName name ++ ": {\n" ++
    concatMap formatIngredient ingredients ++
    concatMap formatSublist sublists ++
    "}\n"
  where
    formatIngredient (Ingredient name qty unit) =
        "    " ++ formatName name ++ ": " ++ show qty ++ " " ++ show unit ++ "\n"
    formatSublist sublist =
        unlines (map ("    " ++) (lines (formatIngredientList sublist)))

instance Show Name where
    show (WordName name) = name
    show (NumberName number) = show number
    show (StringName str) = str

instance Show Quantity where
    show (Quantity qty) = show qty

-- Helper functions for parsing
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f a b = do
    v1 <- a
    f v1 <$> b

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f a b c = do
    v1 <- a
    v2 <- b
    f v1 v2 <$> c

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f a b c d = do
    v1 <- a
    v2 <- b
    v3 <- c
    f v1 v2 v3 <$> d

and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5' f a b c d e = do
    v1 <- a
    v2 <- b
    v3 <- c
    v4 <- d
    f v1 v2 v3 v4 <$> e

and7' :: (a -> b -> c -> d -> e -> f -> g -> h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h
and7' f a b c d e g h = do
    v1 <- a
    v2 <- b
    v3 <- c
    v4 <- d
    v5 <- e
    v6 <- g
    f v1 v2 v3 v4 v5 v6 <$> h

parseChar :: Char -> Parser Char
parseChar c = do
    input <- lift S.get
    let input' = dropWhile isSpace input
    case input' of
        [] -> throwE $ "Cannot find " ++ [c] ++ " in an empty input"
        (h:t) -> if c == h then lift (S.put t) >> return c else throwE $ c : " is not found in " ++ [h]

parseWord :: Parser String
parseWord = do
    input <- lift S.get
    let letters = takeWhile isLetter input
        rest = drop (length letters) input
    if not (null letters)
        then lift (S.put rest) >> return letters
        else throwE $ input ++ " does not start with a letter"

parseNumber :: Parser Int
parseNumber = do
    input <- lift S.get
    let digits = takeWhile isDigit input
        rest = drop (length digits) input
    if null digits
        then throwE "not a number"
        else lift (S.put rest) >> return (read digits)

parseName :: Parser Name
parseName = do
    input <- lift S.get
    let skipWhitespace = dropWhile (== ' ')
        trimmedInput = skipWhitespace input
    case S.runState (runExceptT parseNumber) trimmedInput of
        (Right number, rest) -> lift (S.put rest) >> return (NumberName number)
        (Left _, _) -> case S.runState (runExceptT parseWord) trimmedInput of
            (Right word, rest) -> lift (S.put rest) >> return (WordName word)
            (Left err, _) -> throwE err

parseQuantity :: Parser Quantity
parseQuantity = do
    input <- lift S.get
    let skipWhitespace = dropWhile (== ' ')
        trimmedInput = skipWhitespace input
    case S.runState (runExceptT parseNumber) trimmedInput of
        (Right n, rest) -> lift (S.put (skipWhitespace rest)) >> return (Quantity n)
        (Left err, _) -> throwE err

parseUnit :: Parser Unit
parseUnit = do
    input <- lift S.get
    let skipWhitespace = dropWhile (== ' ')
        trimmedInput = skipWhitespace input
    case span isLetter trimmedInput of
        ("cup", rest) -> lift (S.put rest) >> return Cup
        ("cups", rest) -> lift (S.put rest) >> return Cups
        ("tbsp", rest) -> lift (S.put rest) >> return Tbsp
        ("tsp", rest) -> lift (S.put rest) >> return Tsp
        ("oz", rest) -> lift (S.put rest) >> return Oz
        ("lb", rest) -> lift (S.put rest) >> return Lb
        ("g", rest) -> lift (S.put rest) >> return G
        ("kg", rest) -> lift (S.put rest) >> return Kg
        ("ml", rest) -> lift (S.put rest) >> return Ml
        ("l", rest) -> lift (S.put rest) >> return L
        ("pinch", rest) -> lift (S.put rest) >> return Pinch
        ("cloves", rest) -> lift (S.put rest) >> return Cloves
        ("full", rest) -> lift (S.put rest) >> return Full
        ("half", rest) -> lift (S.put rest) >> return Half
        _ -> throwE "Invalid unit"

parseIngredient :: Parser Ingredient
parseIngredient = and4' (\name _ qty unit -> Ingredient name qty unit) parseName (parseChar ':') parseQuantity (parseChar ' ' *> parseUnit)

-- Parser for IngredientList
parseIngredientList :: Parser IngredientList
parseIngredientList = and5' (\name _ _ (ingredients, nestedLists) _ -> IngredientList name ingredients nestedLists) parseName (parseChar ':') (parseChar '{') parseMoreItems (parseChar '}')

parseMoreItems :: Parser ([Ingredient], [IngredientList])
parseMoreItems = do
    input <- lift S.get
    case S.runState (runExceptT parseIngredient) input of
        (Right ing, rest) -> do
            lift (S.put rest)
            case S.runState (runExceptT (parseChar ',')) rest of
                (Right _, rest') -> do
                    lift (S.put rest')
                    (ings, lists) <- parseMoreItems
                    return (ing : ings, lists)
                (Left _, _) -> return ([ing], [])
        (Left _, _) -> case S.runState (runExceptT parseIngredientList) input of
            (Right list, rest) -> do
                lift (S.put rest)
                case S.runState (runExceptT (parseChar ',')) rest of
                    (Right _, rest') -> do
                        lift (S.put rest')
                        (ings, lists) <- parseMoreItems
                        return (ings, list : lists)
                    (Left _, _) -> return ([], [list])
            (Left _, _) -> return ([], [])

parseQuery :: String -> Either String Query
parseQuery input =
    case S.runState (runExceptT parseCreate) input of
        (Right query, _) -> Right query
        (Left err1, _) -> case S.runState (runExceptT parseAdd) input of
            (Right query, _) -> Right query
            (Left err2, _) -> case S.runState (runExceptT parseRemove) input of
                (Right query, _) -> Right query
                (Left err3, _) -> case S.runState (runExceptT parseGet) input of
                    (Right query, _) -> Right query
                    (Left err4, _) -> case S.runState (runExceptT parseCreateEmptyList) input of
                        (Right query, _) -> Right query
                        (Left err5, _) -> case S.runState (runExceptT parseDelete) input of
                            (Right query, _) -> Right query
                            (Left err6, _) -> case S.runState (runExceptT parseGetList) input of
                                (Right query, _) -> Right query
                                (Left err7, _) -> case S.runState (runExceptT parseCreateList) input of
                                    (Right query, _) -> Right query
                                    (Left err8, _) -> case S.runState (runExceptT parseFind) input of
                                        (Right query, _) -> Right query
                                        (Left err9, _) -> case S.runState (runExceptT parseAddList) input of
                                            (Right query, _) -> Right query
                                            (Left err10, _) -> Left $ "Parse errors: " ++ err8 ++ ", " ++ err9 ++ ", " ++ err10

parseCreate :: Parser Query
parseCreate = and7' create (string "create(") parseName (parseChar ',') parseQuantity (parseChar ',') parseUnit (parseChar ')')
    where create _ name _ qty _ unit _ = Create name qty unit

parseAdd :: Parser Query
parseAdd = and5' add (string "add(") parseName (parseChar ',') parseName (parseChar ')')
    where add _ name _ listName _ = Add name listName

parseAddList :: Parser Query
parseAddList = and5' add (string "add_list(") parseName (parseChar ',') parseName (parseChar ')')
    where add _ name _ listName _ = AddList name listName

parseRemove :: Parser Query
parseRemove = and5' remove (string "remove(") parseName (parseChar ',') parseName (parseChar ')')
    where remove _ name _ listName _ = Remove name listName

parseGet :: Parser Query
parseGet = and2' (\_ name -> Get name) (string "get(") (parseName <* parseChar ')')

parseCreateEmptyList :: Parser Query
parseCreateEmptyList = and2' (\_ name -> CreateEmptyList name) (string "create_empty_list(") (parseName <* parseChar ')')

parseDelete :: Parser Query
parseDelete = and2' (\_ name -> Delete name) (string "delete(") (parseName <* parseChar ')')

parseGetList :: Parser Query
parseGetList = and2' (\_ name -> GetList name) (string "get_list(") (parseName <* parseChar ')')

parseCreateList :: Parser Query
parseCreateList = and3' (\_ ingredientList _ -> CreateList ingredientList) (string "create_list(") parseIngredientList (parseChar ')')

parseFind :: Parser Query
parseFind = and3' (\_ ingredient _ -> Find ingredient) (string "find(") parseIngredient (parseChar ')')

string :: String -> Parser String
string str = do
    input <- lift S.get
    if str `isPrefixOf` input
        then lift (S.put (drop (length str) input)) >> return str
        else throwE $ "Expected '" ++ str ++ "', but got '" ++ take (length str) input ++ "'"

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