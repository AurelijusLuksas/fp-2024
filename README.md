# fp-2024

# Domain
Ingredient list maker. 

# Commands
 - add (ingredient | ingredient_list) - Adds an ingredient or ingredient list to the selected list.
 - create (ingredient) - Creates an ingredient which you can add to lists.
 - remove (ingredient | ingredient_list) - Removes an ingredient or ingredient list from the selected list.
 - get (ingredient | ingredient_list) - Returns an ingredient or igredient list.
 - select (ingredient | ingredient_list) - Selects a list or ingredient to work with.
 - update (ingredient | ingredient_list) (updated_ingredient | updated_ingredient_list) - Updates the selected ingredient or list.
 - create_list (name) - Creates a new list.
 - delete (ingredient | ingredient_list) - Deletes already existing list or ingredient.

# BNF
``` 
<ingredient_list> ::= <name> ": {" <more_items> "}" 
<more_items> ::= <ingredient> ", " <more_items> | <ingredient_list> ", " <more_items> | <ingredient> | <ingredient_list>
<ingredient> ::= <name> ": " <quantity> " " <unit>
<quantity> ::= <number>
<unit> ::= "cup" | "cups" | "tbsp" | "tsp" | "oz" | "lb" | "g" | "kg" | "ml" | "l" | "pinch" | "cloves" | "full" | "half"
<name> ::= <string> | <string> <name>
<number> ::= <digit> | <digit> <number>
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<string> ::= <letter> | <letter> <string>
<letter> ::= "a" | "b" | "c" | ... | "y" | "z" | "A" | "B" | "C" | ... | "Y" | "Z" | " "

<commands> ::= <create> | <add> | <remove> | <get>  | <create_list> | <get_list> | <delete> | create_empty_list> | <find>
<create> ::= "create (" <name> ", " <quantity> ", " <unit> ")"
<add> ::= "add (" <name> ", " <name> ")"
<remove> ::= "remove (" <name> ", " <name> ")" 
<get> ::= "get (" <name> ")" 
<create_list> ::= "create_list (" <ingredient_list> ")"
create_empty_list ::= "create_empty_list (" <name> ")"
find::= "find(" <ingredient> ")" 
<get_list> ::= "get_list (" <name> ")" 
<delete> ::= "delete (" <name> ")"
```

# Example

```

Tomato Pasta {
    Pasta: 200 g
    Sauce: {
        Canned: tomatoes 400 g
        Olive oil: 2 tbsp
        Garlic: 2 cloves
        Spice mix: {
            Salt: 2 pinch
            Black pepper: 1 pinch
            Basil: 1 pinch
        }
        Onion: 1 full
        Parmesan cheese: 50 g
    }
}


create_list(pasta)
create(Honey, 6, tbsp)
find(banana: 3 full)
delete(pasta)
create_list(food: {banana: 3 full, meat:{beef: 100 g}})
remove(banana,food)
create_empty_list(meal)
create(apple,12,cloves)
add(apple,meal)
get_list(meal)
get(apple)

```