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
    response <- W.post "http://localhost:3000" (pack $ unlines  ("BEGIN" : commands ++ ["END"]))
    print response
    return "Success"

collectCommands :: D.Program a -> [String]
collectCommands (Pure _) = []
collectCommands (Free (D.Create name qty unit next)) = ("create(" ++ name ++ ", " ++ show qty ++ ", " ++ unit ++ ")") : collectCommands next
collectCommands (Free (D.Add ingName listName next)) = ("add(" ++ ingName ++ ", " ++ listName ++ ")") : collectCommands next
collectCommands (Free (D.Remove ingName listName next)) = ("remove(" ++ ingName ++ ", " ++ listName ++ ")") : collectCommands next
collectCommands (Free (D.CreateEmptyList name next)) = ("create_empty_list(" ++ name ++ ")") : collectCommands next
collectCommands (Free (D.Delete name next)) = ("delete(" ++ name ++ ")") : collectCommands next
collectCommands (Free (D.CreateList name items next)) = ("create_list(" ++ name ++ ", " ++ show items ++ ")") : collectCommands next


-- Interpreter for testing (in-memory)
type InMemoryState = StateT [(String, String)] (ExceptT String IO)

interpretInMemory :: D.Program a -> InMemoryState a
interpretInMemory (Pure a) = return a
interpretInMemory (Free (D.Create name qty unit next)) = do
    modify ((name, show qty ++ " " ++ unit) :)
    interpretInMemory next
interpretInMemory (Free (D.Add ingName listName next)) = do
    -- Implement in-memory add logic
    interpretInMemory next
interpretInMemory (Free (D.Remove ingName listName next)) = do
    -- Implement in-memory remove logic
    interpretInMemory next
interpretInMemory (Free (D.CreateEmptyList name next)) = do
    -- Implement in-memory create empty list logic
    interpretInMemory next
interpretInMemory (Free (D.Delete name next)) = do
    modify (filter ((/= name) . fst))
    interpretInMemory next
interpretInMemory (Free (D.CreateList name items next)) = do
    -- Implement in-memory create list logic
    interpretInMemory next



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
    _ <- interpretBatch program
    let program = do 
            D.create "banana" 100 "g"
            D.add "banana" "fruits"
    _ <- interpretOneByOne program
    let program = do
            D.save
    result <- interpretOneByOne program
    print result