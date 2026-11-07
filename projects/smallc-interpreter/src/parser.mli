open SmallCTypes
open TokenTypes

val parse_expr : token list -> token list * expr
val parse_stmt : token list -> token list * stmt
val parse_defs : token list -> token list * defn
