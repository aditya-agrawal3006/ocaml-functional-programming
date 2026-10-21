open Fsm.Nfa
open Fsm.Regexp
open Fsm.Utils

let string_of_int_list lst =
  "[" ^ String.concat ";" (List.map string_of_int lst) ^ "]"

let init_str =
  "digraph G { \n rankdir=LR; "
  ^ string_of_int (Hashtbl.hash "-1")
  ^ " [style=\"invis\"]; \n"

let end_str = "\n}"

let nodup x lst = if List.mem x lst then lst else x :: lst

let string_of_vtx _ lst =
  List.fold_left
    (fun acc (v, f) ->
      let shape = if f then "doublecircle" else "circle" in
      acc
      ^ Printf.sprintf "%d [label=\"%s\",shape=%s];\n" (Hashtbl.hash v) v shape
      )
    "" lst

let string_of_ed _ lst =
  List.fold_left
    (fun acc ((s1, _), c, _, (s2, _)) ->
      acc
      ^ Printf.sprintf "%d -> %d [label=\"%s\"];\n" (Hashtbl.hash s1)
          (Hashtbl.hash s2) c )
    "" lst

let write_nfa_to_graphviz (show : 'q -> string) (nfa : ('q, char) nfa_t) : bool
    =
  let name = "output.viz" in
  let (_, _, nfa_q0, nfa_fs, nfa_delta) = nfa in
  let ss, fs, ts = (nfa_q0, nfa_fs, nfa_delta) in
  let sv = (show ss, List.mem ss fs) in
  let vt, ed =
    List.fold_left
        (fun (vt, ed) s ->
        let c = s.input in 
        let v1 = fst s.states in 
        let v2 = snd s.states in 

        let v1' = (show v1, List.mem v1 fs) in
        let v2' = (show v2, List.mem v2 fs) in
        let c' = match c with None -> "ε" | Some x -> String.make 1 x in
       let pair = List.mem {input=c; states=(v2, v1)} ts in
        let e = (v1', c', pair, v2') in
        (nodup v2' (nodup v1' vt), nodup e ed) )
      ([], []) ts
  in
  let ed = (("-1", false), " ", false, (show ss, List.mem ss fs)) :: ed in
  let dot =
    init_str ^ string_of_vtx show (sv :: vt) ^ string_of_ed show ed ^ end_str
  in
  let file = open_out_bin name in
  output_string file dot ;
  flush file ;
  Sys.command (Printf.sprintf "dot %s -Tpng -o output.png && rm %s" name name)
  = 0

;;
print_string "Type regexp to visualize: "

let line = read_line ()

;;
print_string "Convert to DFA (y/n)? "

let line2 = read_line ()

let nfa = nfa_of_string line

;;
if line2 = "n" then
  if write_nfa_to_graphviz string_of_int nfa then
    print_string "Success! Open 'output.png' to see your visualized NFA.\n"
  else
    print_string
      "Failure! Are you sure you have graphviz installed on your machine?\n"
else if write_nfa_to_graphviz string_of_int_list (nfa_to_dfa nfa) then
  print_string "Success! Open 'output.png' to see your visualized DFA.\n"
else
  print_string
    "Failure! Are you sure you have graphviz installed on your machine?\n"
