type person = { name : string; age : int; hobbies : string list; } [@@deriving show { with_path = false }]
type db

val newDatabase : db
val insert : person -> db -> db
val remove : string -> db -> db

type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list-> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

val query : condition -> db -> person list

type comparator = person -> person -> int

val sort : (person -> person -> int) -> db -> person list
val queryBy : condition -> db -> comparator -> person list
val update : condition -> db -> (person -> person) -> db
