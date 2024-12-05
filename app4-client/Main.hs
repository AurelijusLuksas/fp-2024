module Main (main) where

import qualified DSL as D
import qualified Network.Wreq as W
import Data.String.Conversions (cs)
import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Data.ByteString.Lazy.Char8 (pack)

-- Interpreter that sends requests one-by-one
interpretOneByOne :: D.Program a -> IO a
interpretOneByOne (Pure a) = return a
interpretOneByOne (Free (D.Create name qty unit next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "create(" ++ name ++ ", " ++ show qty ++ ", " ++ unit ++ ")")
    interpretOneByOne next
interpretOneByOne (Free (D.Add ingName listName next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "add(" ++ ingName ++ ", " ++ listName ++ ")")
    interpretOneByOne next
interpretOneByOne (Free (D.Remove ingName listName next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "remove(" ++ ingName ++ ", " ++ listName ++ ")")
    interpretOneByOne next
interpretOneByOne (Free (D.CreateEmptyList name next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "create_empty_list(" ++ name ++ ")")
    interpretOneByOne next
interpretOneByOne (Free (D.Delete name next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "delete(" ++ name ++ ")")
    interpretOneByOne next
interpretOneByOne (Free (D.CreateList name items next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "create_list(" ++ name ++ ", " ++ show items ++ ")")
    interpretOneByOne next

interpretOneByOne (Free (D.Save next)) = do
    _ <- W.post "http://localhost:3000" (pack "save")
    interpretOneByOne next
interpretOneByOne (Free (D.Load next)) = do
    _ <- W.post "http://localhost:3000" (pack "load")
    interpretOneByOne next


-- Interpreter that batches commands
interpretBatch :: D.Program a -> IO String
interpretBatch program = do
    let commands = collectCommands program
    _ <- W.post "http://localhost:3000" (pack $ unlines  ("BEGIN" : commands ++ ["END"]))
    return "Success"

collectCommands :: D.Program a -> [String]
collectCommands (Pure _) = []
collectCommands (Free (D.Create name qty unit next)) = ("create(" ++ name ++ ", " ++ show qty ++ ", " ++ unit ++ ")") : collectCommands next
collectCommands (Free (D.Add ingName listName next)) = ("add(" ++ ingName ++ ", " ++ listName ++ ")") : collectCommands next
collectCommands (Free (D.Remove ingName listName next)) = ("remove(" ++ ingName ++ ", " ++ listName ++ ")") : collectCommands next
collectCommands (Free (D.CreateEmptyList name next)) = ("create_empty_list(" ++ name ++ ")") : collectCommands next
collectCommands (Free (D.Delete name next)) = ("delete(" ++ name ++ ")") : collectCommands next
collectCommands (Free (D.CreateList name items next)) = ("create_list(" ++ name ++ ", " ++ show items ++ ")") : collectCommands next
collectCommands (Free (D.Save next)) = "save" : collectCommands next
collectCommands (Free (D.Load next)) = "load" : collectCommands next

interpretProgram :: D.Program a -> IO a
interpretProgram program = do
    let commands = collectCommands program
    if length commands > 1
        then do
            putStrLn "Using batch interpreter"
            interpretBatch program
            return undefined -- Adjust this to return the correct type
        else do
            putStrLn "Using one-by-one interpreter"
            interpretOneByOne program


-- Interpreter for testing (in-memory)
type InMemoryState = StateT [(String, String)] (ExceptT String IO)

interpretInMemory :: D.Program a -> InMemoryState a
interpretInMemory (Pure a) = return a
interpretInMemory (Free (D.Create name qty unit next)) = do
    modify ((name, show qty ++ " " ++ unit) :)
    interpretInMemory next
interpretInMemory (Free (D.Add ingName listName next)) = do
    modify (addToList ingName listName)
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
interpretInMemory (Free (D.CreateList name items next)) = do
    modify ((name, show items) :)
    interpretInMemory next
interpretInMemory (Free (D.Save next)) = do
    -- Implement
    interpretInMemory next
interpretInMemory (Free (D.Load next)) = do
    -- Implement
    interpretInMemory next

addToList :: String -> String -> [(String, String)] -> [(String, String)]
addToList ingName listName = map (\(name, items) -> if name == listName then (name, items ++ ", " ++ ingName) else (name, items))

removeFromList :: String -> String -> [(String, String)] -> [(String, String)]
removeFromList ingName listName = map (\(name, items) -> if name == listName then (name, unwords $ filter (/= ingName) (words items)) else (name, items))


main :: IO ()
main = do
    -- Example usage of the DSL
    let program = do
            D.create "apple" 42 "cup"
            D.create "strawberry" 100 "g"
            D.createEmptyList "fruits"
            D.add "strawberry" "fruits"
            D.add "apple" "fruits"
            D.remove "apple" "fruits"
            D.delete "apple"
    _ <- interpretProgram program
    let program = do 
            D.create "banana" 100 "g"
    _ <- interpretProgram program
    let program = do
            D.add "banana" "fruits"
    _ <- interpretProgram program        
    let program = do
            D.save
    result <- interpretProgram program
    print result

    -- let program = do
    --         D.load
    -- result <- interpretProgram program
    -- print result