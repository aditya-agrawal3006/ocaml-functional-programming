open List
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference

  type ('q, 's) transition = {
    input: 's option; 
    states: 'q * 'q;
  }

  (** NFA type *)
  type ('q,'s) nfa_t = ('s list * 'q list * 'q * 'q list * ('q, 's) transition list)
*)

(****************)
(* NFA simulation *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let (sigma, _, _, _, delta) = nfa in
  match s with
    | Some c when not (elem c sigma) -> []
    | _ -> List.fold_left 
           (fun acc x -> let (a,b) = x.states in
                         if elem a qs && x.input = s then insert b acc
                                                     else acc)
           []
           delta

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec helper states = 
    let next = move nfa states None in
    let all = union states next in
    if eq states all then states
    else helper all
  in helper qs

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let (_, _, ss, fs, _) = nfa in
  let rec helper states c = 
    match c with
    | [] -> List.exists (fun x -> elem x fs) states
    | h :: t -> let next = move nfa states (Some h) in
                helper (e_closure nfa next) t
  in helper (e_closure nfa [ss]) (explode s)


(*******************************)
(* Determinization through subset construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let (sigma, _,_,_,_) = nfa in 
  List.map (fun c -> e_closure nfa (move nfa qs (Some c))) sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  let (sigma, _,_,_,_) = nfa in
  List.map (fun c -> let next = move nfa qs (Some c) in
                     let fs = e_closure nfa next in
                     {input = Some c; states = (qs, fs)}) sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let (_, _, _, fs, _) = nfa in 
  if intersection qs fs = [] then [] 
                             else [qs]

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
    | [] -> dfa
    | h :: t -> let (sigma, qs, q0, fs, delta) = dfa in
                let ns = new_states nfa h in
                let unique = diff (union [] ns) qs in
                let qs2 = union qs ns in
                let fs2 = union fs (new_finals nfa h) in
                let delta2 = union delta (new_trans nfa h) in
                nfa_to_dfa_step nfa (sigma, qs2, q0, fs2, delta2) (t @ unique)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let (sigma, _, q0, _, _) = nfa in
  let ss = e_closure nfa [q0] in
  let dfa = (sigma, [ss], ss, [], []) in
  nfa_to_dfa_step nfa dfa [ss]

(*******************************)
(* DFA to regular expression  *)
(*******************************)

let dfa_to_regex (dfa: ('q, char) nfa_t) : regexp_t =
  let (_, qs, q0, fs, delta) = dfa in
  let states = List.sort_uniq compare
    (q0 :: (qs @ fs @ List.concat_map
      (fun t -> let (a, b) = t.states in [a; b]) delta)) in
  let indexed = List.mapi (fun i q -> (q, i)) states in
  let index q = List.assoc q indexed in
  let start = List.length states in
  let finish = start + 1 in

  (* Missing edges represent the empty language. *)
  let get edges a b = List.assoc_opt (a, b) edges in
  let union_regex a b =
    match a, b with
    | None, r | r, None -> r
    | Some x, Some y when x = y -> Some x
    | Some x, Some y -> Some (Union (x, y))
  in
  let concat_regex a b =
    match a, b with
    | None, _ | _, None -> None
    | Some Empty_String, r | r, Some Empty_String -> r
    | Some x, Some y -> Some (Concat (x, y))
  in
  let star_regex r =
    match r with
    | None | Some Empty_String -> Some Empty_String
    | Some (Star _ as x) -> Some x
    | Some x -> Some (Star x)
  in
  let add edges a b r =
    let combined = union_regex (get edges a b) (Some r) in
    let remaining = List.remove_assoc (a, b) edges in
    match combined with
    | None -> remaining
    | Some value -> ((a, b), value) :: remaining
  in

  (* Add one entry/exit state so there is a single path to reduce. *)
  let edges = List.fold_left
    (fun acc t ->
      let (a, b) = t.states in
      let r = match t.input with
        | None -> Empty_String
        | Some c -> Char c
      in add acc (index a) (index b) r)
    [] delta
  in
  let edges = add edges start (index q0) Empty_String in
  let edges = List.fold_left
    (fun acc q -> add acc (index q) finish Empty_String) edges fs in

  (* Remove each original state, combining every path through it. *)
  let rec eliminate edges pending =
    match pending with
    | [] ->
        (match get edges start finish with
         | Some r -> r
         | None -> failwith "DFA recognizes the empty language")
    | k :: rest ->
        let kept = start :: finish :: rest in
        let loop = star_regex (get edges k k) in
        let next = List.fold_left
          (fun acc i ->
            List.fold_left
              (fun acc j ->
                let via = concat_regex
                  (concat_regex (get edges i k) loop)
                  (get edges k j) in
                match via with
                | None -> acc
                | Some r -> add acc i j r)
              acc kept)
          edges kept in
        let next = List.filter
          (fun ((a, b), _) -> a <> k && b <> k) next in
        eliminate next rest
  in
  eliminate edges (List.map snd indexed)
