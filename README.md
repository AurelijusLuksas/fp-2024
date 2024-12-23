# fp-2024

# Domain
Ingredient list maker. 

# Commands
 - add(name, list_name) - Adds an ingredient to list
 - add_list(list_name, list_name) - Adds a list to list
 - create(name, quantity, unit) - Creates an ingredient.
 - create_list(list_name) - Creates a list.
 - remove(name, list_name) - Removes an ingredient or ingredient list from the selected list.
 - get(name) - Returns an ingredient.
 - get_list(list_name) - Return an ingrediet_list.
 - find(ingredient) - finds particular ingredient in lists.
 - delete(name | list_name) - Deletes already existing list or ingredient.

# BNF
``` 
<batch> ::= 'BEGIN' <commands_list> 'END'

<commands_list> ::= <commands> ';' <commands_list> | <commands>

<load_commands> ::= 'load'
<save_commands> ::= 'save'

<ingredient_list> ::= <name> ": {" <more_items> "}" 
<more_items> ::= <ingredient> ", " <more_items> | <ingredient_list> ", " <more_items> | <ingredient> | <ingredient_list>
<ingredient> ::= <name> ": " <quantity> " " <unit>
<quantity> ::= <number>
<unit> ::= "cup" | "cups" | "tbsp" | "tsp" | "oz" | "lb" | "g" | "kg" | "ml" | "l" | "pinch" | "cloves" | "full" | "half"
<name> ::= <string> | <string> <name>
<number> ::= <digit> | <digit> <number>
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<string> ::= <letter> | <letter> <string>
<letter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | " "

<commands> ::= <create> 
            | <add> 
            | <add_list>
            | <remove> 
            | <get>  
            | <create_list> 
            | <get_list> 
            | <delete> 
            | <create_empty_list> 
            | <find> 
            | <load_commands> 
            | <save_commands>

<create> ::= "create (" <name> ", " <quantity> ", " <unit> ")"
<add> ::= "add (" <name> ", " <name> ")"
<add_list> ::= "add_list (" <name> ", " <name> ")"
<remove> ::= "remove (" <name> ", " <name> ")" 
<get> ::= "get (" <name> ")" 
<create_list> ::= "create_list (" <ingredient_list> ")"
<create_empty_list> ::= "create_empty_list (" <name> ")"
<find> ::= "find(" <ingredient> ")" 
<get_list> ::= "get_list (" <name> ")" 
<delete> ::= "delete (" <name> ")"
```

# Tests

During the development of Lab 4, I encountered issues with several tests failing after changing the parser implementation to use `ExceptT String (State String) a`

So a decission was made to remove some of the older tests, to save time instead of fixing all of them.

# Example

```
lab2_example.txt
lab3_example.txt
lab4_example.txt
```

