# incomplete grammar
```
program  ::= (item)* EOF
item     ::= func
func     ::= ("export" | "import")* "func" IDENT "(" (func_arg)* ")" block?
func_arg ::= IDENT ":" TYPE ","

expr      ::= expr_unit | binary
expr_unit ::= return | block | func | func_call | unary | literal | "(" expr ")" | IDENT
return    ::= "return" expr
block     ::= "{" (expr)* "}"
func_call ::= IDENT "(" (expr ",")* ")"
binary    ::= expr_unit binop expr_unit
 binop     ::= "+" | "-" | "*" | "/"
unary     ::= uop expr
 uop       ::= "-" | "!"
literal   ::= NUMBER | STRING | "true" | "false"
```
