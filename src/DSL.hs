{-# LANGUAGE DeriveFunctor #-}
module DSL where

import Control.Monad.Free (Free(..), liftF)

-- Define the DSL commands
data Command next = Create String Int String next
                  | Add String String next
                  | Remove String String next
                  | Get String (String -> next)
                  | CreateEmptyList String next
                  | Delete String next
                  deriving Functor

-- Define the Program type
type Program = Free Command

-- DSL operations
create :: String -> Int -> String -> Program ()
create name qty unit = liftF $ Create name qty unit ()

add :: String -> String -> Program ()
add ingName listName = liftF $ Add ingName listName ()

remove :: String -> String -> Program ()
remove ingName listName = liftF $ Remove ingName listName ()

get :: String -> Program String
get name = liftF $ Get name id

createEmptyList :: String -> Program ()
createEmptyList name = liftF $ CreateEmptyList name ()

delete :: String -> Program ()
delete name = liftF $ Delete name ()