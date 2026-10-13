type person = { name : string; age : int; hobbies : string list }
[@@deriving show { with_path = false }]

type comparator = person -> person -> int
type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

(* TODO: Implement functions below *)

type db  = person list

let newDatabase = []

let insert person db = person :: db

let rec filter f l = 
    match l with
      | [] -> []
      | h :: t -> if f h then h :: filter f t else filter f t

let remove name db = filter (fun person -> person.name <> name) db

let rec satisfies condition person =
  match condition with
  | True ->
      true

  | False ->
      false

  | Age f ->
      f person.age

  | Name f ->
      f person.name

  | Hobbies f ->
      f person.hobbies

  | And (c1, c2) ->
      satisfies c1 person && satisfies c2 person

  | Or (c1, c2) ->
      satisfies c1 person || satisfies c2 person

  | Not c ->
      not (satisfies c person)

  | If (c1, c2, c3) ->
      if satisfies c1 person then
        satisfies c2 person
      else
        satisfies c3 person

let query condition db = filter (fun person -> satisfies condition person) db

let sort comparator db = List.sort comparator db

let queryBy condition db comparator = sort comparator (query condition db)

let update condition db change = List.map (fun person -> if satisfies condition person
                                                         then change person
                                                        else person) db
