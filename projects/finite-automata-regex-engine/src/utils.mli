val elem : 'a -> 'a list -> bool
val insert : 'a -> 'a list -> 'a list
val insert_all : 'a list -> 'a list -> 'a list
val subset : 'a list -> 'a list -> bool
val eq : 'a list -> 'a list -> bool
val remove : 'a -> 'a list -> 'a list
val minus : 'a list -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val intersection : 'a list -> 'a list -> 'a list
val product : 'a list -> 'b list -> ('a * 'b) list
val diff : 'a list -> 'a list -> 'a list
val cat : 'a -> 'b list -> ('a * 'b) list
val explode : string -> char list
val fresh : unit -> int

exception IllegalExpression of string

type ('q, 's) transition = {
  input: 's option;
  states: 'q * 'q
}
[@@deriving show { with_path = false }]

type ('q,'s) nfa_t = ('s list * 'q list * 'q * 'q list * ('q, 's) transition list)
[@@deriving show { with_path = false }]

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t
[@@deriving show { with_path = false }]

type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

val regexp_to_string : regexp_t -> string
val str2re : string -> Re.re
val tokenize : string -> token list
val tok_to_str : token -> string
val parse_regexp : token list -> regexp_t
