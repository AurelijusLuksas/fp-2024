module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["recipe", "ingredient_list", "more_ingredients", "more_ingredient_lists", "ingredient", "quantity", "unit", "name"]
