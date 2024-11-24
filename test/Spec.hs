{-# LANGUAGE ImportQualifiedPost #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
    parseGetList
    )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lib2 Tests"
  [ unitTests
  , parserTests
  , stateTransitionTests
  ]

-- Unit Tests for Parser Functions
unitTests :: TestTree
unitTests = testGroup "Parser Functions"
  [ testCase "parseName - WordName" $
      Lib2.parseName "apple" @?= Right (Lib2.WordName "apple", "")
  
  , testCase "parseName - NumberName" $
      Lib2.parseName "123" @?= Right (Lib2.NumberName 123, "")
  
  , testCase "parseQuantity - valid quantity" $
      Lib2.parseQuantity " 42" @?= Right (Lib2.Quantity 42, "")
  
  , testCase "parseQuantity - invalid quantity" $
      Lib2.parseQuantity "abc" @?= Left "not a number"
  
  , testCase "parseUnit - valid unit" $
      Lib2.parseUnit "cup" @?= Right (Lib2.Cup, "")
  
  , testCase "parseUnit - invalid unit" $
      Lib2.parseUnit "invalid" @?= Left "Invalid unit"
  
  , testCase "parseQuery - Create query" $
      Lib2.parseQuery "create(apple, 42, cup)" @?= Right (Lib2.Create (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup)
  
  , testCase "parseQuery - Add query" $
      Lib2.parseQuery "add(apple, fruits)" @?= Right (Lib2.Add (Lib2.WordName "apple") (Lib2.WordName "fruits"))
  
  , testCase "parseQuery - Remove query" $
      Lib2.parseQuery "remove(apple, fruits)" @?= Right (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "fruits"))
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

  , testCase "stateTransition - Add non-existing ingredient" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 1) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Add (Lib2.WordName "apple") (Lib2.WordName "fruits")) @?= Left "Ingredient or list not found"
  
  , testCase "stateTransition - Remove Ingredient" $
      let state = Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup] []])] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "fruits")) @?= Right (["Removed ingredient"], Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [] []])] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup])
  
  , testCase "stateTransition - Get existing ingredient" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Get (Lib2.WordName "apple")) @?= Right (["apple: 42 Cup"], state)

  , testCase "stateTransition - Delete existing ingredient" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Delete (Lib2.WordName "apple")) @?= Right (["Deleted ingredient"], Lib2.State [] [])

  , testCase "stateTransition - Delete non-existing ingredient" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 1) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Delete (Lib2.WordName "apple")) @?= Left "Ingredient or list not found"

  , testCase "stateTransition - Create Ingredient List" $
      let state = Lib2.emptyState
      in Lib2.stateTransition state (Lib2.CreateEmptyList (Lib2.WordName "fruits")) @?= Right (["Created empty ingredient list"], Lib2.State [(Lib2.WordName "fruits", [Lib2.IngredientList (Lib2.WordName "fruits") [] []])] [])

  , testCase "stateTransition - Add Ingredient to non-existing list" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Add (Lib2.WordName "apple") (Lib2.WordName "nonExistingList")) @?= Left "Ingredient or list not found"

  , testCase "stateTransition - Remove Ingredient from non-existing list" $
      let state = Lib2.State [] [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup]
      in Lib2.stateTransition state (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "nonExistingList")) @?= Left "Ingredient or list not found"

  , testCase "stateTransition - Get non-existing list" $
      let state = Lib2.State [] []
      in Lib2.stateTransition state (Lib2.GetList (Lib2.WordName "nonExistingList")) @?= Left "List not found"  

  , testCase "stateTransition - Get List which contains list" $
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
      in Lib2.stateTransition state (Lib2.GetList (Lib2.WordName "meal")) @?= Right (["meal: {\n\tbread: 100 G\n\tfruit: {\n\t\tbanana: 3 Full\n\t}\n\n}","Found list"], state)

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
      in Lib2.stateTransition state (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "fruits")) @?= Right (["Removed ingredient"], Lib2.State [(Lib2.WordName "meal", [Lib2.IngredientList (Lib2.WordName "meal") [] [Lib2.IngredientList (Lib2.WordName "fruits") [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup] []]] )] [])
    
    , testCase "stateTransition - Create Ingredient List" $
        let state = Lib2.State [] []
            ingredientList = Lib2.IngredientList (Lib2.WordName "meal") 
                            [Lib2.Ingredient (Lib2.WordName "bread") (Lib2.Quantity 100) Lib2.G] 
                            [Lib2.IngredientList (Lib2.WordName "fruit") 
                                [Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 3) Lib2.Full] 
                                []]
            expectedState = Lib2.State [(Lib2.WordName "meal", [ingredientList])] []
        in Lib2.stateTransition state (Lib2.CreateList ingredientList) @?= Right (["Created ingredient list"], expectedState)
  
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

  , testCase "stateTransition - Get nested list" $
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
      in Lib2.stateTransition state (Lib2.GetList (Lib2.WordName "meal")) @?= Right (["meal: {\n\tbread: 100 G\n\tfruit: {\n\t\tbanana: 3 Full\n\t}\n\n}","Found list"], state)

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

-- Additional parser tests for helper functions
parserTests :: TestTree
parserTests = testGroup "Parser Helper Functions"
  [ testCase "parseIngredient - valid ingredient" $
      Lib2.parseIngredient "apple: 42 cloves" @?= Right (Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cloves, "")
  
  , testCase "parseIngredient - invalid ingredient" $
      Lib2.parseIngredient "apple: abc cloves" @?= Left "not a number"
  
  , testCase "parseIngredientList - valid ingredient list" $
      Lib2.parseIngredientList "fruits: {apple: 42 cup, banana: 100 g}" @?= Right (Lib2.IngredientList (Lib2.WordName "fruits") [Lib2.Ingredient (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup, Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 100) Lib2.G] [], "")
  
  , testCase "parseCreate - valid create query" $
      Lib2.parseCreate "create(apple, 42, cup)" @?= Right (Lib2.Create (Lib2.WordName "apple") (Lib2.Quantity 42) Lib2.Cup, "")
  
  , testCase "parseCreate - invalid create query" $
      Lib2.parseCreate "create(apple, abc, cup)" @?= Left "not a number"
  
  , testCase "parseAdd - valid add query" $
      Lib2.parseAdd "add(apple, fruits)" @?= Right (Lib2.Add (Lib2.WordName "apple") (Lib2.WordName "fruits"), "")
  
  , testCase "parseRemove - valid remove query" $
      Lib2.parseRemove "remove(apple, fruits)" @?= Right (Lib2.Remove (Lib2.WordName "apple") (Lib2.WordName "fruits"), "")
  
  , testCase "parseGet - valid get query" $
      Lib2.parseGet "get(apple)" @?= Right (Lib2.Get (Lib2.WordName "apple"), ")")
  
  , testCase "parseGet - invalid get query" $
      Lib2.parseGet "get(123)" @?= Right (Lib2.Get (Lib2.NumberName 123), ")")
  
  , testCase "parseCreateEmptyList - valid create list query" $
      Lib2.parseCreateEmptyList "create_empty_list(fruits)" @?= Right (Lib2.CreateEmptyList (Lib2.WordName "fruits"), ")")
  
  , testCase "parseCreateEmptyList - invalid create list query" $
      Lib2.parseCreateEmptyList "create_empty_list(123)" @?= Right (Lib2.CreateEmptyList (Lib2.NumberName 123), ")")
  
  , testCase "parseGetList - valid get list query" $
      Lib2.parseGetList "get_list(fruits)" @?= Right (Lib2.GetList (Lib2.WordName "fruits"), ")")
  
  , testCase "parseGetList - invalid get list query" $
      Lib2.parseGetList "get_list(123)" @?= Right (Lib2.GetList (Lib2.NumberName 123), ")")
  
  , testCase "parseDelete - valid delete query" $
      Lib2.parseDelete "delete(apple)" @?= Right (Lib2.Delete (Lib2.WordName "apple"), ")")
  
  , testCase "parseDelete - invalid delete query" $
      Lib2.parseDelete "delete(123)" @?= Right (Lib2.Delete (Lib2.NumberName 123), ")")

  , testCase "parseCreateList - valid create list query" $
      Lib2.parseCreateList "create_list(meal: {bread: 100 g, fruit:{banana: 3 full}})" @?= Right (Lib2.CreateList (Lib2.IngredientList (Lib2.WordName "meal") [Lib2.Ingredient (Lib2.WordName "bread") (Lib2.Quantity 100) Lib2.G] [Lib2.IngredientList (Lib2.WordName "fruit") [Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 3) Lib2.Full] []]), "")

  , testCase "parseCreateList - invalid create list query" $
      Lib2.parseCreateList "create_list(meal:{ bread: 100 G, fruit: {banana: 3 Full}" @?= Left "} is not found in b"

  , testCase "parseFind - valid find query" $
      Lib2.parseFind "find(banana: 3 full)" @?= Right (Lib2.Find (Lib2.Ingredient (Lib2.WordName "banana") (Lib2.Quantity 3) Lib2.Full), "")

  ]
