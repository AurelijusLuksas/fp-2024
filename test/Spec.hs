{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified (
        Query(..),
      State(..),
      Name(..),
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
      )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib Tests"
  [ testGroup "Lib2 parseQuery tests"
      [ testCase "Parse empty input should give error" $
          parseQuery "" @?= Left "Input received:  | Parse errors: Expected 'create(', Expected 'add(', Expected 'remove(', Expected 'get(', Expected 'select(', Expected 'update(', Expected 'create_list(', Expected 'delete('"
      ],
    testGroup "Lib2 parseUnit tests"
      [ testCase "Parse 'cup' unit" $
          parseUnit "cup" @?= Right (Cup, "")
      , testCase "Parse 'tbsp' unit" $
          parseUnit "tbsp" @?= Right (Tbsp, "")
      , testCase "Parse invalid unit" $
          parseUnit "invalid" @?= Left "Invalid unit"
      ],
    testGroup "Lib2 stateTransition tests"
      [ testCase "Create ingredient" $
          stateTransition emptyState (Create (WordName "Sugar") (Quantity 1) Cup) @?= Right (["Created ingredient"], State [] [Ingredient (WordName "Sugar") (Quantity 1) Cup])
      , testCase "Add ingredient" $
          stateTransition (State [] [Ingredient (WordName "Sugar") (Quantity 1) Cup]) (Add (WordName "Sugar")) @?= Right (["Added ingredient"], State [] [Ingredient (WordName "Sugar") (Quantity 1) Cup, Ingredient (WordName "Sugar") (Quantity 1) Cup])
      , testCase "Remove ingredient" $
          stateTransition (State [] [Ingredient (WordName "Sugar") (Quantity 1) Cup]) (Remove (WordName "Sugar")) @?= Right (["Removed ingredient"], State [] [])
      , testCase "Get ingredient" $
          stateTransition (State [] [Ingredient (WordName "Sugar") (Quantity 1) Cup]) (Get (WordName "Sugar")) @?= Right (["Got ingredient"], State [] [Ingredient (WordName "Sugar") (Quantity 1) Cup])
      , testCase "Select ingredient" $
          stateTransition emptyState (Select (Ingredient (WordName "Sugar") (Quantity 1) Cup)) @?= Right (["Selected ingredient"], State [] [Ingredient (WordName "Sugar") (Quantity 1) Cup])
      , testCase "Update ingredient" $
          stateTransition (State [] [Ingredient (WordName "Sugar") (Quantity 1) Cup]) (Update (WordName "Sugar") (WordName "Brown Sugar") (Quantity 2) Tbsp) @?= Right (["Updated ingredient"], State [] [Ingredient (WordName "Brown Sugar") (Quantity 2) Tbsp])
      , testCase "Create ingredient list" $
          stateTransition emptyState (CreateList (WordName "Groceries")) @?= Right (["Created list"], State [(WordName "Groceries", [])] [])
      , testCase "Delete ingredient list" $
          stateTransition (State [(WordName "Groceries", [])] []) (Delete (WordName "Groceries")) @?= Right (["Deleted list"], State [] [])
      ]
  ]