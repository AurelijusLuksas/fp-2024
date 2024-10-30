{-# LANGUAGE InstanceSigs #-}
module Lib2( 
  Query(..),
  State(..),
  Name(..),
  parseQuery,
  emptyState,
  stateTransition
) where

import Data.Char (isDigit, isAlphaNum, isLetter)
import Data.List (isPrefixOf)
import GHC.ForeignPtr (Finalizers(HaskellFinalizers))

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.


data Name = NumName Int 
          | WordName String 
          | StringName String 
          deriving (Show, Eq)

newtype Quantity = Quantity Int
  deriving (Show, Eq)

data Unit = Cup | Cups | Tbsp | Tsp | Oz | Lb | G | Kg | Ml | L | Pinch | Full | HaskellFinalizers
  deriving (Show, Eq)

data Ingredient = Ingredient Name Quantity Unit
  deriving (Show, Eq)

data IngredientList = IngredientList Name [Ingredient] [IngredientList]
  deriving (Show, Eq)

-- Define Query
data Query 
    = Create Name Quantity Unit
    | Add Name
    | Remove Name
    | Get Name
    | Select Name
    | Update Name Quantity Unit
    | CreateList Name
    | Delete Name
    deriving (Show, Eq)

type Parser a = String -> Either String (a, String)

-- Helper functions for parsing

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f a b input = 
  case a input of
    Right (v1, r1) -> case b r1 of
      Right (v2, r2) -> Right (f v1 v2, r2)
      Left e2 -> Left e2
    Left e1 -> Left e1

or2 :: Parser a -> Parser a -> Parser a
or2 a b input = 
  case a input of
    Right r1 -> Right r1
    Left e1 -> case b input of
      Right r2 -> Right r2
      Left e2 -> Left (e1 ++ ", " ++ e2)

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in empty input")
parseChar c (h:t) = if c == h then Right (c, t) else Left (c : "is not found in " ++ [h])

parseWord :: Parser String
parseWord input =
  let letters = taleWhile isLetter input
      rest = drop (length letters) input
  in if not (null letters) 
      then Right (letters, rest)
      else Left (input ++ "does not start with a letter")    

parseNumber :: Parser Int
parseNumber [] = Left "Empty input"
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
parseQuantity = fmap Quantity . parseNumber

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
parseIngredient = and4' Ingredient parseName (parseChar ':') parseQuantity parseUnit

parseIngredientList :: Parser IngredientList
parseIngredientList = and4' IngredientList parseName (parseChar ':') (parseChar '{') parseIngredients (parseChar '}')

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
parseCreate = and4' (\_ name _ -> Create name) (string "create (") parseName (parseChar ',') parseQuantity parseUnit (parseChar ')')

parseAdd :: Parser Query
parseAdd = and2' (\_ name -> Add name) (string "add (") parseName (parseChar ')')

parseRemove :: Parser Query
parseRemove = and2' (\_ name -> Remove name) (string "remove (") parseName (parseChar ')')

parseGet :: Parser Query
parseGet = and2' (\_ name -> Get name) (string "get (") parseName (parseChar ')')

parseSelect :: Parser Query
parseSelect = and2' (\_ ing -> Select ing) (string "select (") parseIngredient (parseChar ')')

parseUpdate :: Parser Query
parseUpdate = and4' (\_ name1 _ -> Update name1) (string "update (") parseName (parseChar ',') parseName parseQuantity parseUnit (parseChar ')')

parseCreateList :: Parser Query
parseCreateList = and2' (\_ name -> CreateList name) (string "create_list (") parseName (parseChar ')')

parseDelete :: Parser Query
parseDelete = and2' (\_ name -> Delete name) (string "delete (") parseName (parseChar ')')



-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""

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
