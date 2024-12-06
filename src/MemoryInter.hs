-- MemoryInter.hs
module MemoryInter (interpretInMemory) where

import qualified DSL as D
import Control.Monad.Free (Free(..))  
import Control.Monad.Trans.State.Strict (StateT, get, modify)
import Control.Monad.Except (ExceptT, liftIO)

type InMemoryState = StateT [(String, String)] (ExceptT String IO)

interpretInMemory :: D.Program a -> InMemoryState a
interpretInMemory (Pure a) = return a
interpretInMemory (Free (D.Create name qty unit next)) = do
    modify ((name, show qty ++ " " ++ unit) :)
    interpretInMemory next
interpretInMemory (Free (D.Add ingName listName next)) = do
    modify (addToList ingName listName)
    interpretInMemory next
interpretInMemory (Free (D.AddList listName lisName next)) = do
    modify (addListToList listName lisName)
    interpretInMemory next
interpretInMemory (Free (D.Remove ingName listName next)) = do
    modify (removeFromList ingName listName)
    interpretInMemory next
interpretInMemory (Free (D.CreateEmptyList name next)) = do
    modify ((name, "") :)
    interpretInMemory next
interpretInMemory (Free (D.Delete name next)) = do
    modify (filter ((/= name) . fst))
    interpretInMemory next
interpretInMemory (Free (D.Save next)) = do
    state <- get
    _ <- liftIO $ writeFile "statetest.txt" (show state)
    interpretInMemory next
interpretInMemory (Free (D.Load next)) = do
    content <- liftIO $ readFile "statetest.txt"
    let state = read content
    modify (const state)
    interpretInMemory next

addToList :: String -> String -> [(String, String)] -> [(String, String)]
addToList ingName listName = map (\(name, items) -> if name == listName then (name, if null items then ingName else items ++ ", " ++ ingName) else (name, items))

addListToList :: String -> String -> [(String, String)] -> [(String, String)]
addListToList listName parentListName = map (\(name, items) -> if name == parentListName then (name, if null items then listName else items ++ ", " ++ listName) else (name, items))

removeFromList :: String -> String -> [(String, String)] -> [(String, String)]
removeFromList ingName listName = map (\(name, items) -> if name == listName then (name, unwords $ filter (/= ingName) (words items)) else (name, items))