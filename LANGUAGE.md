# incomplete grammar
```
program  ::= (item)* EOF
item     ::= func
func     ::= ("export" | "import")* "func" IDENT "(" (func_arg)* ")" block?
func_arg ::= type_ascription ","

type_ascription ::= IDENT ":" TYPE

stmt        ::= declaration | return | break | continue | expr
declaration ::= "let" (IDENT | type_ascription) "=" expr
return      ::= "return" expr
break       ::= "break"
continue    ::= "continue"
block       ::= "{" (stmt)* "}"

expr      ::= expr_unit | binary
expr_unit ::= if | while | block | unary | group | literal | func_call | IDENT | assign

binary    ::= expr_unit binop expr_unit
binop     ::= "+" | "-" | "*" | "/"
unary     ::= uop expr
uop       ::= "-" | "!"

if        ::= "if" expr block ("else" (if | block))?
while     ::= "while" expr block
group     ::= "(" expr ")"
literal   ::= NUMBER | STRING | "true" | "false"
func_call ::= IDENT "(" (expr ",")* ")
assign    ::= expr_unit "=" expr_unit"
assignop  ::= expr_unit binop "=" expr_unit
```

# roadmap
[x] variable mutability
[x] function calls
[x] while loops
[x] comparison ops (<, <=, >, >=, etc.)
[x] break and continue keywords
[x] else if branches
[x] operator equals (+=, -=, etc.)
[x] start function
[x] import functions
[ ] import namespace and export name specification
[ ] specify variable mutability (semantic analysis?)
[ ] evaluatable blocks (blocks return a value)
[ ] optionals
