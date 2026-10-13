type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf
[@@deriving show {with_path = false}]

val tree_fold : ('a -> 'b -> 'a -> 'a) -> 'a -> 'b tree -> 'a
val map : 'a tree -> ('a -> 'b) -> 'b tree
val in_order : 'a tree -> 'a list
val pre_order : 'a tree -> 'a list
val compose : ('a -> 'a) tree -> 'a -> 'a
val depth : 'a tree -> int
val trim : 'a tree -> int -> 'a tree