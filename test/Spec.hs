{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assert)
import Test.Tasty.QuickCheck as QC
import Control.Concurrent (newChan, Chan)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, newTVar, readTVar, readTVarIO, newTVarIO, TVar)
import Control.Monad (liftM3)
import Debug.Trace (trace)
import Data.List (sortOn)
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Test.QuickCheck.Monadic as QCM
import qualified DSL as D

import qualified MemoryInter as I 

import Lib1 qualified
import Lib2 qualified (
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
    )
import Lib3 qualified (
    Command(..),
    Statements(..),
    StorageOp(..),
    stateTransition,
    parseCommand,
    parseStatements,
    renderStatements,
    marshallState
    )

import Test.Tasty.QuickCheck (Testable(property))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State.Strict (State, runState, execStateT)
import Test.QuickCheck.Test (test)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lib2 Tests"
  [ unitTests
  , stateTransitionTests
  , propertyTests
  , memoryIntegrationTests
  ]

-- Helper function to run the parser
runParser :: Lib2.Parser a -> String -> Either String (a, String)
runParser p input = 
    let (result, remaining) = runState (runExceptT p) input
    in case result of
        Left err -> Left err
        Right value -> Right (value, remaining)

unitTests :: TestTree
unitTests = testGroup "Parser Functions"
  [ testCase "parseName - WordName" $
      runParser Lib2.parseName "apple" @?= Right (Lib2.WordName "apple", "")
  
  , testCase "parseName - NumberName" $
      runParser Lib2.parseName "123" @?= Right (Lib2.NumberName 123, "")
  
  , testCase "parseQuantity - valid quantity" $
      runParser Lib2.parseQuantity " 42" @?= Right (Lib2.Quantity 42, "")
  
  , testCase "parseQuantity - invalid quantity" $
      runParser Lib2.parseQuantity "abc" @?= Left "not a number"
  
  , testCase "parseUnit - valid unit" $
      runParser Lib2.parseUnit "cup" @?= Right (Lib2.Cup, "")
  
  , testCase "parseUnit - invalid unit" $
      runParser Lib2.parseUnit "invalid" @?= Left "Invalid unit"
  
  , testCase "parseCreate - Create query" $
      runParser Lib2.parseCreate "create(apple, 42, cup)" @?= Right (Lib2.Create (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup, "")
  
  , testCase "parseAdd - Add query" $
      runParser Lib2.parseAdd "add(apple, fruits)" @?= Right (Lib2.Add (Lib2.WordName "apple") (Lib2.WordName "fruits"), "")
  
  , testCase "parseRemove - Remove query" $
      runParser Lib2.parseRemove "remove(apple, fruits)" @?= Right (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "fruits"), "")
  
  , testCase "parseGet - Get query" $
      runParser Lib2.parseGet "get(apple)" @?= Right (Lib2.Get (Lib2.WordName "apple"), "")
  
  , testCase "parseGet - Get query with number" $
      runParser Lib2.parseGet "get(123)" @?= Right (Lib2.Get (Lib2.NumberName 123), "")
  
  , testCase "parseCreateEmptyList - CreateEmptyList query" $
      runParser Lib2.parseCreateEmptyList "create_empty_list(fruits)" @?= Right (Lib2.CreateEmptyList (Lib2.WordName "fruits"), "")
  
  , testCase "parseCreateEmptyList - CreateEmptyList query with number" $
      runParser Lib2.parseCreateEmptyList "create_empty_list(123)" @?= Right (Lib2.CreateEmptyList (Lib2.NumberName 123), "")
  
  , testCase "parseGetList - GetList query" $
      runParser Lib2.parseGetList "get_list(fruits)" @?= Right (Lib2.GetList (Lib2.WordName "fruits"), "")
  
  , testCase "parseGetList - GetList query with number" $
      runParser Lib2.parseGetList "get_list(123)" @?= Right (Lib2.GetList (Lib2.NumberName 123), "")
  
  , testCase "parseDelete - Delete query" $
      runParser Lib2.parseDelete "delete(apple)" @?= Right (Lib2.Delete (Lib2.WordName "apple"), "")
  
  , testCase "parseDelete - Delete query with number" $
      runParser Lib2.parseDelete "delete(123)" @?= Right (Lib2.Delete (Lib2.NumberName 123), "")

  ]
-- State Transition Tests
stateTransitionTests :: TestTree
stateTransitionTests = testGroup "State Transition"
  [ testCase "stateTransition - Create Ingredient" $
      let state = Lib2.emptyState
      in Lib2.stateTransition state (Lib2.Create (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup) @?= Right (["Created ingredient"], Lib2.State [] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup])

 , testCase "stateTransition - Add existing ingredient" $
      let state = Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [] []])] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Add (Lib2.WordName "apple") (Lib2.WordName "fruits")) @?= Right (["Added ingredient to list"], Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup] []])] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup])

  , testCase "stateTransition - Remove Ingredient" $
      let state = Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup] []])] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "fruits")) @?= Right (["Removed ingredient"], Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [] []])] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup])
  
  , testCase "stateTransition - Get existing ingredient" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Get (Lib2.WordName "apple")) @?= Right (["apple: 42 Cup"], state)

  , testCase "stateTransition - Delete existing ingredient" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Delete (Lib2.WordName "apple")) @?= Right (["Deleted ingredient"], Lib2.State [] [])

  , testCase "stateTransition - Create Ingredient List" $
      let state = Lib2.emptyState
      in Lib2.stateTransition state (Lib2.CreateEmptyList (Lib2.WordName "fruits")) @?= Right (["Created empty ingredient list"], Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [] []])] [])

  , testCase "stateTransition - Remove Ingredient from non-existing list" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "nonExistingList")) @?= Left "Ingredient or list not found"

  , testCase "stateTransition - Get non-existing ingredient" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 1) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Get (Lib2.WordName "apple")) @?= Left "Ingredient not found"

  , testCase "stateTransition - Create Ingredient List with existing name" $
      let state = Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [] []])] []
      in Lib2.stateTransition state (Lib2.CreateEmptyList (Lib2.WordName "fruits")) @?= Left "List already exists"

  , testCase "stateTransition - Remove ingredient from nested list" $
      let state = Lib2.State 
                    [(Lib2.WordName "meal", 
                      [Lib2.IngredientList (Lib2.WordName "meal") 
                        [] 
                        [Lib2.IngredientList (Lib2.WordName "fruits") [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup] []]
                      ]
                    )] 
                    []
      in Lib2.stateTransition state (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "fruits")) @?= Right (["Removed ingredient"], Lib2.State [(Lib2.WordName "meal", [Lib2.IngredientList (Lib2.WordName "meal") [] [Lib2.IngredientList (Lib2.WordName "fruits") [] []]] )] [])

  , testCase "stateTransition - Find ingredient in lists" $
    let state = Lib2.State 
                  [(Lib2.WordName "meal", 
                    [Lib2.IngredientList (Lib2.WordName "meal") 
                      [Lib2.Ingredient (Lib2.WordName "bread") (Lib2.Quantity 100) Lib2.G] 
                      [Lib2.IngredientList (Lib2.WordName "fruit") 
                        [Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 3) Lib2.Full] 
                        []]
                    ]
                  )] 
                  []
    in Lib2.stateTransition state (Lib2.Find (Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 3) Lib2.Full)) @?= Right (["Found in lists: meal"], state)

  , testCase "stateTransition - Delete nested list" $
      let state = Lib2.State 
                    [(Lib2.WordName "meal", 
                      [Lib2.IngredientList (Lib2.WordName "meal") 
                        [] 
                        [Lib2.IngredientList (Lib2.WordName "fruits") [] []]
                      ]
                    )] 
                    []
      in Lib2.stateTransition state (Lib2.Delete (Lib2.WordName "fruits")) @?= Right (["Deleted list"], Lib2.State [(Lib2.WordName "meal", [Lib2.IngredientList (Lib2.WordName "meal") [] []])] [])

  , testCase "stateTransition - Delete non-existing list" $
      let state = Lib2.State [] []
      in Lib2.stateTransition state (Lib2.Delete (Lib2.WordName "nonExistingList")) @?= Left "Ingredient or list not found"
  ]

propertyTests :: TestTree
propertyTests = testGroup "Lib3 Property Tests"
  [
    QC.testProperty "Save then load" $ withMaxSuccess 100 saveThenLoad,
    QC.testProperty "Preserve state" $ withMaxSuccess 100 preserveState
  ]

genLimitedStatements :: Gen Lib3.Statements
genLimitedStatements = do
  n <- choose (1, 4) -- Limit the number of commands to a maximum of 4
  stmts <- vectorOf n genQuery
  return $ Lib3.Batch stmts

saveThenLoad :: Property
saveThenLoad = forAll genLimitedStatements $ \stmts ->
  let rendered = Lib3.renderStatements stmts
      parsed = Lib3.parseStatements rendered
  in 
  counterexample (showDetails stmts rendered parsed) $
  case parsed of
    Right (parsedStmts, _) -> stmts == parsedStmts
    _ -> False
  where
    showDetails expected rendered actual =
      "Expected: " ++ show expected ++
      "\nRendered: " ++ rendered ++
      "\nActual: " ++ show actual


preserveState :: Property
preserveState = forAll genLimitedStatements $ \stmts -> ioProperty $ do
  oldState <- newTVarIO Lib2.emptyState
  newState <- newTVarIO Lib2.emptyState
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  _ <- Lib3.stateTransition oldState (Lib3.StatementCommand stmts) chan
  marshalled <- Lib3.marshallState <$> readTVarIO oldState
  let rendered = Lib3.renderStatements marshalled
  case Lib3.parseStatements rendered of
    Left err -> return $ counterexample ("Parse error: " ++ err) False
    Right (parsed, _) -> do
      _ <- Lib3.stateTransition newState (Lib3.StatementCommand parsed) chan
      oldStateVal <- readTVarIO oldState
      newStateVal <- readTVarIO newState
      return $ counterexample (showDetails oldStateVal newStateVal) (sortState oldStateVal == sortState newStateVal)
  where
    showDetails expected actual =
      "Expected: " ++ show expected ++
      "\nActual: " ++ show actual

    sortState state = state 
      { Lib2.ingredients = sortOn show (Lib2.ingredients state)
      , Lib2.ingredientLists = sortOn (show . fst) (Lib2.ingredientLists state)
      }

-- Generators

maxLen :: Int
maxLen = 15

limListOf1 :: Gen a -> Gen [a]
limListOf1 genElement = sized $ \n ->
  let size = min n maxLen
  in resize size (listOf1 genElement)

genStatements :: Gen Lib3.Statements
genStatements = oneof [
    Lib3.Single <$> genQuery,
    Lib3.Batch <$> limListOf1 genQuery
  ]

genQuery :: Gen Lib2.Query
genQuery = oneof [
    Lib2.Create <$> genName <*> genQuantity <*> genUnit,
    Lib2.Add <$> genName <*> genName,
    Lib2.CreateEmptyList <$> genName,
    Lib2.Delete <$> genName,
    Lib2.Remove <$> genName <*> genName
  ]

genName :: Gen Lib2.Name
genName = oneof [
    Lib2.WordName <$> (listOf1 (elements ['a'..'z']) `suchThat` (\s -> length s > 1))
  ]

genQuantity :: Gen Lib2.Quantity
genQuantity = Lib2.Quantity <$> (arbitrary `suchThat` (> 0))

genUnit :: Gen Lib2.Unit
genUnit = elements [Lib2.Cup, Lib2.Cups, Lib2.Tbsp, Lib2.Tsp, Lib2.Oz, Lib2.Lb, Lib2.G, Lib2.Kg, Lib2.Ml, Lib2.L, Lib2.Pinch, Lib2.Cloves, Lib2.Full, Lib2.Half]

memoryIntegrationTests :: TestTree
memoryIntegrationTests = testGroup "Memory Integration Tests"
  [ testCase "Create" $ do
      let program = D.create "apple" 42 "cloves"
      result <- runInMemory program
      result @?= Right [("apple", "42 cloves")]

  , testCase "Add" $ do
      let program = do
            D.create "apple" 42 "cloves"
            D.createEmptyList "fruits"
            D.add "apple" "fruits"
      result <- runInMemory program
      result @?= Right [("fruits", "apple"), ("apple", "42 cloves")]

  , testCase "Remove" $ do
      let program = do
            D.create "apple" 42 "cloves"
            D.createEmptyList "fruits"
            D.add "apple" "fruits"
            D.remove "apple" "fruits"
      result <- runInMemory program
      result @?= Right [("fruits", ""), ("apple", "42 cloves")]

  , testCase "AddList" $ do
      let program = do
            D.create "apple" 42 "cloves"
            D.createEmptyList "fruits"
            D.add "apple" "fruits"
            D.createEmptyList "meal"
            D.addList "fruits" "meal"
      result <- runInMemory program
      result @?= Right [("meal", "fruits"), ("fruits", "apple"), ("apple", "42 cloves")]

  , testCase "Delete" $ do
      let program = do
            D.create "apple" 42 "cloves"
            D.delete "apple"
      result <- runInMemory program
      result @?= Right []

  , testCase "Save and Load" $ do
      let program = do
            D.create "apple" 42 "cloves"
            D.save
            D.delete "apple"
            D.load
      result <- runInMemory program
      result @?= Right [("apple", "42 cloves")]
  ]

runInMemory :: D.Program a -> IO (Either String [(String, String)])
runInMemory program = runExceptT $ execStateT (I.interpretInMemory program) []