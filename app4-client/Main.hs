module Main (main) where

import qualified DSL as D
import qualified Network.Wreq as W
import Data.String.Conversions (cs)
import Control.Lens
import Control.Monad.Free (Free(..))
import Control.Monad.State (StateT, get, modify, MonadState (state))
import Control.Monad.Except (ExceptT, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import qualified MemoryInter as I

-- Interpreter that sends requests one-by-one
interpretOneByOne :: D.Program a -> IO a
interpretOneByOne (Pure a) = return a
interpretOneByOne (Free (D.Create name qty unit next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "create(" ++ name ++ ", " ++ show qty ++ ", " ++ unit ++ ")")
    interpretOneByOne next
interpretOneByOne (Free (D.Add ingName listName next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "add(" ++ ingName ++ ", " ++ listName ++ ")")
    interpretOneByOne next
interpretOneByOne (Free (D.AddList listName lisName next)) = do
    _ <- W.post "http://localhost:3000" (pack $ "add_list(" ++ listName ++ ", " ++ lisName ++ ")")
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
collectCommands (Free (D.AddList listName lisName next)) = ("add_list(" ++ listName ++ ", " ++ lisName ++ ")") : collectCommands next
collectCommands (Free (D.Remove ingName listName next)) = ("remove(" ++ ingName ++ ", " ++ listName ++ ")") : collectCommands next
collectCommands (Free (D.CreateEmptyList name next)) = ("create_empty_list(" ++ name ++ ")") : collectCommands next
collectCommands (Free (D.Delete name next)) = ("delete(" ++ name ++ ")") : collectCommands next
collectCommands (Free (D.Save next)) = "save" : collectCommands next
collectCommands (Free (D.Load next)) = "load" : collectCommands next

interpretProgram :: D.Program a -> IO a
interpretProgram program = do
    let commands = collectCommands program
    if length commands > 1
        then do
            putStrLn "Using batch interpreter"
            interpretBatch program
            return undefined 
        else do
            putStrLn "Using one-by-one interpreter"
            interpretOneByOne program


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
            D.createEmptyList "meal"
            D.create "bread" 200 "g"
            D.add "bread" "meal"
            D.addList "fruits" "meal"
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