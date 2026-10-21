open List
open Nfa
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference:

  type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t
*)

(*******************************)
(* Thompson-style regular expression construction *)
(** Build an epsilon-NFA from a regular-expression tree using Thompson's construction. *)
let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t = 
  match regexp with
    | Empty_String -> let ss = fresh () in
                      let fs = fresh () in
                      ([], [ss;fs], ss, [fs], [{input = None; states = (ss,fs)}])

    | Char c -> let ss = fresh () in
                let fs = fresh () in
                ([c], [ss;fs], ss, [fs], [{input = Some c; states = (ss,fs)}])

    | Union (r1, r2) -> let (sigma1, qs1, q01, fs1, delta1) = regexp_to_nfa r1 in
                        let (sigma2, qs2, q02, fs2, delta2) = regexp_to_nfa r2 in
                        let ss = fresh () in
                        let fs = fresh () in
                        let split = [{input = None; states = (ss,q01)}; {input = None; states = (ss,q02)}] in
                        let connects = List.map (fun x -> {input = None; states = (x, fs)}) (fs1@fs2) in
                        (union sigma1 sigma2, ([ss;fs] @ qs1 @ qs2), ss, [fs], split @ delta1 @ delta2 @ connects)

    | Concat (r1, r2) ->  let (sigma1, qs1, q01, fs1, delta1) = regexp_to_nfa r1 in
                          let (sigma2, qs2, q02, fs2, delta2) = regexp_to_nfa r2 in
                          let connects = List.map (fun x -> {input = None; states = (x, q02)}) fs1 in
                          (union sigma1 sigma2, qs1 @ qs2, q01, fs2,  delta1 @ connects @ delta2)

    | Star r -> let (sigma1, qs1, q01, fs1, delta1) = regexp_to_nfa r in
                let ss = fresh () in
                let fs = fresh () in
                let start = [{input = None; states = (ss, q01)}; {input = None; states = (ss, fs)}] in
                let again = List.map (fun x -> {input = None; states = (x, q01)}) fs1 in
                let finish = List.map (fun x -> {input = None; states = (x, fs)}) fs1 in
                (sigma1, [ss;fs] @ qs1, ss, [fs],  start @ delta1 @ again @ finish)

(* Parsing and presentation helpers *)
(** Tokenize and parse a string into a regular-expression tree. *)
let regexp_of_string str = parse_regexp @@ tokenize str

(** Convert a regular-expression tree back into a string. *)
let string_of_regexp regex = regexp_to_string regex

(** Parse a regular-expression string and build its NFA. *)
let nfa_of_string str = regexp_to_nfa @@ regexp_of_string str
