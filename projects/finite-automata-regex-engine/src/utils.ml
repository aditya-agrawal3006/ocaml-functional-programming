(** Set operations, automata types, and regular-expression parsing.
    The shared interfaces and parsing utilities originate from
    University of Maryland coursework. *)

(* common set operations *) 
(** 
  Returns true iff [x] is an element of the set [a]. 
  @param x element to check
  @param a set to check
  @return [true] iff [x] is an 
          element of [a] 
*)
let rec elem x a =
  match a with
  | h::t -> (h = x) || (elem x t)
  | [] -> false

(** 
  Set insertion. Inserts [x] into the set [a].
  @param x element to insert
  @param a set to insert into
  @return new set containing all 
          elements of [a], as well as [x] 
*)
let rec insert x a =
  if not (elem x a) then x::a else a

(** 
  Inserts each element from [xs] into the set [a].
  @param xs elements to insert
  @param a set to insert into
  @return new set containing all of those elements.
*)
let insert_all xs a =
  List.fold_right insert xs a

(** True if all elements in [a] occur in [b]. *)
let rec subset a b =
  match a with
  | h::t -> (elem h b) && (subset t b)
  | [] -> true

(** Equality and difference for list-based sets. *)
let rec eq a b = (subset a b) && (subset b a)

let rec remove x a =
  match a with
  | h::t -> if h = x then t else h::(remove x t)
  | [] -> []

let rec diff a b =
  match b with
  | [] -> a
  | h::t -> diff (remove h a) t

let rec minus a b = diff a b

let rec union a b =
  match a with
  | h::t -> insert h (union t b)
  | [] ->
    (match b with
     | h::t -> insert h (union [] t)
     | [] -> [])

let rec intersection a b =
  match a with
  | h::t -> if elem h b then insert h (intersection t b) else (intersection t b)
  | [] -> []

let rec cat x a =
  match a with
  | [] -> []
  | h::t -> (x,h)::(cat x t)

let rec product a b =
  let rec product_help x b =
    match b with
    | h::t -> insert (x, h) (product_help x t)
    | [] -> [] in
  match a with
  | h::t -> union (product_help h b) (product t b)
  | [] -> []

let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(* Automata data types *)
type ('q, 's) transition = {
  input: 's option; 
  states: 'q * 'q;
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

let rec regexp_to_string r = match r with
   Empty_String -> "E"
  |Char(c) -> String.make 1 c
  |Union(r1,r2) -> "(" ^ regexp_to_string r1 ^ "|" ^ regexp_to_string r2 ^ ")"
  |Concat(r1,r2) -> regexp_to_string r1 ^ regexp_to_string r2
  |Star(r1) -> "(" ^ regexp_to_string r1 ^ ")*"

exception IllegalExpression of string

type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let str2re s = Re.(seq [start; Re.Posix.re s ]) |> Re.compile

let tokenize str =
  let re_toks = [
    (str2re "[a-z]", fun gs -> Some(Tok_Char (Re.Group.get gs 0).[0], 1));
    (str2re "E",     fun _  -> Some(Tok_Epsilon, 1));
    (str2re "\\|",   fun _  -> Some(Tok_Union, 1));
    (str2re "\\*",   fun _  -> Some(Tok_Star, 1));
    (str2re "\\(",   fun _  -> Some(Tok_LParen, 1));
    (str2re "\\)",   fun _  -> Some(Tok_RParen, 1))] in
  let rec helper pos s =
    if pos >= String.length s then [Tok_END]
    else match (List.find_map (fun (re, f) -> Option.bind (Re.exec_opt ~pos re s) f) re_toks) with 
      | None -> raise (IllegalExpression ("tokenize: " ^ s)) 
      | Some(tok, len) -> tok :: helper (pos + len) s
  in
  helper 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")
