# Work-In-Progress Grammar
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
expr_unit ::= if | loop | while | block | unary | group | literal | func_call | IDENT | assign

binary    ::= expr_unit binop expr_unit
binop     ::= "+" | "-" | "*" | "/"
unary     ::= uop expr
uop       ::= "-" | "!"

if        ::= "if" expr block ("else" (if | block))?
loop      ::= "loop" block
while     ::= "while" expr block
group     ::= "(" expr ")"
array_lit ::= "[" (expr ",")* "]"
literal   ::= NUMBER | STRING | "true" | "false" | array_lit
func_call ::= IDENT "(" (expr ",")* ")
assign    ::= expr_unit "=" expr_unit"
assignop  ::= expr_unit binop "=" expr_unit
```
