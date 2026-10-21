(* Regular expression conversion *)
val regexp_to_nfa : Utils.regexp_t -> (int, char) Utils.nfa_t
(* Parsing and formatting helpers *)
val regexp_of_string : string -> Utils.regexp_t
val string_of_regexp : Utils.regexp_t -> string
val nfa_of_string : string -> (int, char) Utils.nfa_t
