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
<ingredient_list> ::= <name> ": {" <more_ingredients> "}" | <name> ": {" <more_ingredients> <more_ingredient_lists> "}" | <name> ": {" <more_ingredient_lists> <more_ingredients> "}" | <name> ": {" <more_ingredients> <more_ingredient_lists> <more_ingredients> "}"
<more_ingredients> ::= <ingredient> | <ingredient> <more_ingredients>
<more_ingredient_lists> ::= <ingredient_list> | <ingredient_list> <more_ingredient_lists>
<ingredient> ::= <name> ": " <quantity> " " <unit> " "
<quantity> ::= <number>
<unit> ::= "cup" | "cups" | "tbsp" | "tsp" | "oz" | "lb" | "g" | "kg" | "ml" | "l" | "pinch" | "cloves" | "full" | "half"
<name> ::= <string> | <string> <name>
<number> ::= <digit> | <digit> <number>
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<string> ::= <letter> | <letter> <string>
<letter> ::= "a" | "b" | "c" | ... | "y" | "z" | "A" | "B" | "C" | ... | "Y" | "Z" | " "

<commands> ::= <create> | <add> | <remove> | <get> | <select> | <update> | <create_list> | <delete>
<create> ::= "create(" <name> ", " <quantity> ", " <unit> ")"
<add> ::= "add(" <name> ")"
<remove> ::= "remove(" <name> ")" 
<get> ::= "get(" <name> ")" 
<select> ::= "select(" <ingredient> ")" | "select (" <name> ")"
<update> ::= "update(" <name> ", " <name> ", " <quantity> ", " <unit> ")" 
<create_list> ::= "create_list(" <name> ")"
<delete> ::= "delete(" <name> ")"
```

# Example

```

Tomato Pasta {
    Pasta: 200 g
    Sauce: {
        Canned: tomatoes 400 g
        Olive oil: 2 tbsp
        Garlic: 2 cloves
        Onion: 1 full
        Salt: 2 pinch
        Black pepper: 1 pinch
        Basil: 1 pinch
        Parmesan: cheese 50 g
    }
}


create_list(pasta)
create(Honey, 6, tbsp)
update(Honey, Sugar, 15, g)
select(Garlic: 3 cloves)
delete(pasta)

```