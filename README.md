# fp-2024

# Domain
Ingredient list maker. 

# Commands
 - add (ingredient | ingredient_list) - Adds an ingredient or ingredient list to the selected list.
 - remove (ingredient | ingredient_list) - Removes an ingredient or ingredient list from the selected list.
 - get (ingredient | ingredient_list) - Returns an ingredient or igredient list.
 - select (ingredient | ingredient_list) - Selects a list or ingredient to work with.
 - update (ingredient | ingredient_list) (updated_ingredient | updated_ingredient_list) - Updates the selected ingredient or list.
 - append (ingredient | ingredient_list) (modified_ingredient | modified_ingredient_list) - Adds modification to the end of ingredient or list.
 - create_list (name) - Creates a new list.
 - delete (ingredient | ingredient_list) - Deletes already existing list or ingredient.

# BNF
``` 
<recipe> ::= <name> | <name> " {" <ingredient_list> "}"
<ingredient_list> ::= "ingredient_list: {" <more_ingredients> "}" | "ingredient_list: {" <more_ingredients> <more_ingredient_lists> "}" | "ingredient_list: {" <more_ingredient_lists> <more_ingredients> "}" | "ingredient_list: {" <more_ingredients> <more_ingredient_lists> <more_ingredients> "}"
<more_ingredients> ::= <ingredient> | <ingredient> <more_ingredients>
<more_ingredient_lists> ::= <ingredient_list> | <ingredient_list> <more_ingredient_lists>
<ingredient> ::= "ingredient: " <name> " " <quantity> " " <unit> " "
<quantity> ::= <number>
<unit> ::= "cup" | "cups" | "tbsp" | "tsp" | "oz" | "lb" | "g" | "kg" | "ml" | "l" | "pinch" | "cloves" | "full" | "half"
<name> ::= <word> | <word> <name>
<number> ::= <digit> | <digit> <number>
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<word> ::= <letter> | <letter> <word>
<letter> ::= "a" | "b" | "c" | ... | "y" | "z" | "A" | "B" | "C" | ... | "Y" | "Z" | " "
```

# Example

```
Tomato pasta {
    ingredient_list: {
        ingredient: Pasta 200 g
        ingredient_list: {
            ingredient: Canned tomatoes 400 g
            ingredient: Olive oil 2 tbsp
            ingredient: Garlic 2 cloves
            ingredient: Onion 1 full
            ingredient: Salt 2 pinch
            ingredient: Black pepper 1 pinch
            ingredient: Basil 1 pinch
            ingredient: Parmesan cheese 50 g
        }
    }
}
```