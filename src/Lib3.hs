{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    Statements (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Command(..),
    ) where

import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import Control.Concurrent.STM ( STM, TVar, atomically, newTVarIO, readTVar, writeTVar, readTVarIO )
import Control.Exception ( SomeException, try )
import qualified Lib2
import Data.List ( isPrefixOf, isSuffixOf, lines, partition )
import Data.Either ( partitionEithers )
import Data.Char (isSpace)
import Control.Monad (foldM)
import Prelude ( String, IO, Either(..), Maybe(..), Char, Show, Eq, Bool(..), foldr, show, (++), (.), lines, reverse, dropWhile, map, concatMap, unlines, null, not, filter, length, take, drop, otherwise, ($), (==), any, elem, concat, return, putStrLn, error, writeFile, readFile, (-), head, tail, fst, either )

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan -- waits for a StorageOp val
  case op of

    Save content notifyChan -> do
      result <- try $ writeFile "state.txt" content
      case result of
        Left err -> putStrLn $ "Error saving state: " ++ show (err :: SomeException) -- if err, msg to the console
        Right _  -> writeChan notifyChan () -- if success, notifyChan sends a signal (()) back to the caller
      storageOpLoop chan -- calls itself - ready to handle more operations

    Load notifyChan -> do
      result <- try $ readFile "state.txt"
      case result of
        Left err      -> writeChan notifyChan $ "Error loading state: " ++ show (err :: SomeException)
        Right content -> writeChan notifyChan content
      storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | "load" `isPrefixOf` input = Right (LoadCommand, drop 4 input)
  | "save" `isPrefixOf` input = Right (SaveCommand, drop 4 input)
  | "BEGIN" `isPrefixOf` input = case parseStatements input of
      Left err -> Left err
      Right (stmts, rest) -> Right (StatementCommand stmts, rest)
  | otherwise = case Lib2.parseQuery (trim input) of
      Left err -> Left err
      Right query -> Right (StatementCommand (Single query), "")

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  let inputTrim = trim input
  in if "BEGIN" `isPrefixOf` inputTrim
    then
      if "END" `isSuffixOf` inputTrim
        then
          let body = trim $ drop 5 $ take (length inputTrim - 3) inputTrim
          in if null body
            then Right (Batch [], "") -- empty batch
            else
              let queries = map (Lib2.parseQuery . trim) (lines body)
                  (errors, parsedQueries) = partitionEithers queries
              in if null errors
                then Right (Batch parsedQueries, "")
                else Left $ "Error parsing queries: " ++ unlines errors
        else Left "Batch must end with 'END'"
    else case Lib2.parseQuery inputTrim of
      Left err -> Left err
      Right query -> Right (Single query, "")

trim :: String -> String
trim = f . f 
  where f = reverse . dropWhile isSpace

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State ingredientLists ingredients) =
    let ingredientQueries = map ingredientToQuery ingredients
        listQueries = concatMap listToQueries ingredientLists
        (createListQueries, addListQueries) = partition isCreateEmptyListQuery listQueries
        allQueries = ingredientQueries ++ createListQueries ++ addListQueries
    in Batch allQueries

ingredientToQuery :: Lib2.Ingredient -> Lib2.Query
ingredientToQuery (Lib2.Ingredient name qty unit) = Lib2.Create name qty unit

listToQueries :: (Lib2.Name, [Lib2.IngredientList]) -> [Lib2.Query]
listToQueries (name, lists) = 
    let createListQuery = Lib2.CreateEmptyList name
        addQueries = concatMap (ingredientListToQueries name) lists
    in createListQuery : addQueries

ingredientListToQueries :: Lib2.Name -> Lib2.IngredientList -> [Lib2.Query]
ingredientListToQueries parentName (Lib2.IngredientList name ingredients sublists) =
    let addIngredientQueries = map (\(Lib2.Ingredient ingName qty unit) -> Lib2.Add ingName parentName) ingredients
        addSublistQueries = map (\(Lib2.IngredientList sublistName _ _) -> Lib2.AddList sublistName parentName) sublists
        nestedSublistQueries = concatMap (ingredientListToQueries name) sublists
    in addIngredientQueries ++ addSublistQueries ++ nestedSublistQueries

isCreateEmptyListQuery :: Lib2.Query -> Bool
isCreateEmptyListQuery (Lib2.CreateEmptyList _) = True
isCreateEmptyListQuery _ = False

renderStatements :: Statements -> String
renderStatements (Batch queries) = "BEGIN\n" ++ unlines (map renderQuery queries) ++ "END"
renderStatements (Single query) = renderQuery query

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.Create name qty unit) = "create(" ++ renderName name ++ ", " ++ renderQuantity qty ++ ", " ++ renderUnit unit ++ ")"
renderQuery (Lib2.Add ingName listName) = "add(" ++ renderName ingName ++ ", " ++ renderName listName ++ ")"
renderQuery (Lib2.AddList listName lisName ) = "add_list(" ++ renderName listName ++ ", " ++ renderName lisName ++ ")"
renderQuery (Lib2.Remove ingName listName) = "remove(" ++ renderName ingName ++ ", " ++ renderName listName ++ ")"
renderQuery (Lib2.Get name) = "get(" ++ renderName name ++ ")"
renderQuery (Lib2.CreateList ingList) = "create_list(" ++ renderIngredientList ingList ++ ")"
renderQuery (Lib2.CreateEmptyList name) = "create_empty_list(" ++ renderName name ++ ")"
renderQuery (Lib2.GetList name) = "get_list(" ++ renderName name ++ ")"
renderQuery (Lib2.Delete name) = "delete(" ++ renderName name ++ ")"
renderQuery (Lib2.Find ingredient) = "find(" ++ renderIngredient ingredient ++ ")"

renderQuantity :: Lib2.Quantity -> String
renderQuantity (Lib2.Quantity n) = show n

renderName :: Lib2.Name -> String
renderName (Lib2.NumberName n) = show n
renderName (Lib2.WordName w) = w
renderName (Lib2.StringName s) = s

renderUnit :: Lib2.Unit -> String
renderUnit Lib2.Cup = "cup"
renderUnit Lib2.Cups = "cups"
renderUnit Lib2.Tbsp = "tbsp"
renderUnit Lib2.Tsp = "tsp"
renderUnit Lib2.Oz = "oz"
renderUnit Lib2.Lb = "lb"
renderUnit Lib2.G = "g"
renderUnit Lib2.Kg = "kg"
renderUnit Lib2.Ml = "ml"
renderUnit Lib2.L = "l"
renderUnit Lib2.Pinch = "pinch"
renderUnit Lib2.Cloves = "cloves"
renderUnit Lib2.Full = "full"
renderUnit Lib2.Half = "half"

renderIngredient :: Lib2.Ingredient -> String
renderIngredient (Lib2.Ingredient name qty unit) = renderName name ++ ": " ++ show qty ++ " " ++ renderUnit unit

renderIngredientList :: Lib2.IngredientList -> String
renderIngredientList (Lib2.IngredientList name ingredients sublists) =
    renderName name ++ ": {\n" ++
    unlines (map (("\t" ++) . renderIngredient) ingredients) ++
    concatMap (("\t" ++) . renderIngredientList) sublists ++ "}"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- Batches must be executed atomically (STM).
-- Right contains an optional message to print and
-- an updated program's state (potentially loaded from a file)
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String [String])
stateTransition stateVar command ioChan = case command of
    StatementCommand stmts -> do
        result <- atomically $ do
            state <- readTVar stateVar
            case executeStatements state stmts of
                Left err -> return $ Left err
                Right newState -> do
                    writeTVar stateVar newState
                    return $ Right newState
        return $ case result of
            Left err -> Left err
            Right newState -> Right ["State updated"]
    LoadCommand -> do
        result <- loadState ioChan
        case result of
            Left err -> return $ Left err
            Right newState -> do
                atomically $ writeTVar stateVar newState
                return $ Right ["State loaded"]
    SaveCommand -> do
        state <- readTVarIO stateVar
        result <- saveState state ioChan
        return $ case result of
            Left err -> Left err
            Right () -> Right ["State saved"]

-- Helper function to execute statements atomically
executeStatements :: Lib2.State -> Statements -> Either String Lib2.State
executeStatements state (Single query) = case Lib2.stateTransition state query of
    Left err -> Left err
    Right (_, newState) -> Right newState
executeStatements state (Batch queries) = foldM executeQuery state queries
  where
    executeQuery st query = case Lib2.stateTransition st query of
        Left err -> Left err
        Right (_, newState) -> Right newState

-- Helper function to load state from file
loadState :: Chan StorageOp -> IO (Either String Lib2.State)
loadState ioChan = do
    resultChan <- newChan
    writeChan ioChan (Load resultChan)
    result <- readChan resultChan
    putStrLn $ "Loaded state:\n" ++ result 
    case parseStatements result of
        Left err -> return $ Left err
        Right (stmts, _) -> case executeStatements Lib2.emptyState stmts of
            Left err -> return $ Left err
            Right newState -> return $ Right newState

-- Helper function to save state to file
saveState :: Lib2.State -> Chan StorageOp -> IO (Either String ())
saveState state ioChan = do
    let content = renderStatements (marshallState state)
    -- putStrLn $ "Saving state:\n" ++ content 
    resultChan <- newChan
    writeChan ioChan (Save content resultChan)
    readChan resultChan
    return $ Right ()