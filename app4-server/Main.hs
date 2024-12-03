{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Web.Scotty
import qualified Lib2
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Control.Concurrent (forkIO, newChan, Chan, readChan, writeChan)
import Lib3 (storageOpLoop, stateTransition, StorageOp(..), parseCommand)

main :: IO ()
main = do
    state <- newTVarIO Lib2.emptyState
    chan <- newChan :: IO (Chan StorageOp)
    _ <- forkIO $ storageOpLoop chan
    scotty 3000 $ do
        post "/" $ do
            b <- body
            liftIO $ putStrLn $ concat ["Request was: ", cs b]
            response <- liftIO $ handleRequest (cs b) state chan
            text $ cs response

handleRequest :: String -> TVar Lib2.State -> Chan StorageOp -> IO String
handleRequest request state chan = do
    case Lib3.parseCommand request of
        Left err -> return $ "Error: " ++ err
        Right (command, _) -> do
            result <- stateTransition state command chan
            case result of
                Left err -> return $ "Error: " ++ err
                Right messages -> return $ unlines messages