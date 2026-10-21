(* NFA operations *)
val e_closure : ('q, 's) Utils.nfa_t -> 'q list -> 'q list
val move : ('q, 's) Utils.nfa_t -> 'q list -> 's option -> 'q list
val accept : ('q, char) Utils.nfa_t -> string -> bool

(* Subset construction *)
val new_states : ('q, 's) Utils.nfa_t -> 'q list -> 'q list list
val new_trans : ('q, 's) Utils.nfa_t -> 'q list -> ('q list, 's) Utils.transition list
val new_finals : ('q, 's) Utils.nfa_t -> 'q list -> 'q list list
val nfa_to_dfa : ('q, 's) Utils.nfa_t -> ('q list, 's) Utils.nfa_t

val dfa_to_regex : ('q, char) Utils.nfa_t -> Utils.regexp_t
