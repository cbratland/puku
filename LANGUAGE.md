# incomplete grammar
```
program  ::= (item)* EOF
item     ::= func
func     ::= ("export" | "import")* "func" IDENT "(" (func_arg)* ")" block?
func_arg ::= type_ascription ","

type_ascription ::= IDENT ":" TYPE

stmt        ::= declaration | return | expr
declaration ::= "let" (IDENT | type_ascription) "=" expr
return      ::= "return" expr
block       ::= "{" (stmt)* "}"

expr      ::= expr_unit | binary
expr_unit ::= block | func | func_call | assign | unary | literal | "(" expr ")" | IDENT
func_call ::= IDENT "(" (expr ",")* ")"
assign    ::= expr_unit "=" expr_unit
binary    ::= expr_unit binop expr_unit
binop     ::= "+" | "-" | "*" | "/"
unary     ::= uop expr
uop       ::= "-" | "!"
literal   ::= NUMBER | STRING | "true" | "false"
```
