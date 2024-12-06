{-# LANGUAGE DeriveFunctor #-}
module DSL (
    Program,
    Command(..),
    create,
    add,
    addList,
    remove,
    createEmptyList,
    delete,
    save,
    load
) where

import Control.Monad.Free (Free(..), liftF)
import qualified Control.Monad.Trans.State.Strict as S (State, get, put, runState)
import qualified Control.Monad.Trans.Except as E (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Alternative(..), many)
import Data.Char (isDigit, isLetter)
import Data.List (isPrefixOf)

-- Define the DSL commands
data Command next = Create String Int String next
                  | Add String String next
                  | AddList String String next
                  | Remove String String next
                  | CreateEmptyList String next
                  | Delete String next
                  | Save next
                  | Load next
                  deriving Functor

-- Define the Program type
type Program = Free Command

-- DSL operations
create :: String -> Int -> String -> Program ()
create name qty unit = liftF $ Create name qty unit ()

add :: String -> String -> Program ()
add ingName listName = liftF $ Add ingName listName ()

addList :: String -> String -> Program ()
addList listName lisName = liftF $ AddList listName lisName ()

remove :: String -> String -> Program ()
remove ingName listName = liftF $ Remove ingName listName ()

createEmptyList :: String -> Program ()
createEmptyList name = liftF $ CreateEmptyList name ()

delete :: String -> Program ()
delete name = liftF $ Delete name ()

save :: Program ()
save = liftF $ Save ()

load :: Program ()
load = liftF $ Load ()
