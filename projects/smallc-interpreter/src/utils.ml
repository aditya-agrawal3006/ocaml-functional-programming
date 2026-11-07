open SmallCTypes
open TokenTypes

(**********************************
 *     Lexer and Parse Utils      *
 **********************************)

let string_of_token = show_token

let string_of_data_type = show_data_type

let string_of_expr = show_expr

let string_of_stmt = show_stmt

let string_of_defn = show_defn

(* General formatting helpers *)
let string_of_list ?newline:(newline=false) (f : 'a -> string) (l : 'a list) : string =
  "[" ^ (String.concat ", " @@ List.map f l) ^ "]" ^ (if newline then "\n" else "");;

(**********************************
 *    Input and Token Helpers      *
 **********************************)

let string_of_in_channel (ic : in_channel) : string =
  let lines : string list =
    let try_read () =
      try Some ((input_line ic) ^ "\n") with End_of_file -> None in
    let rec loop acc = match try_read () with
      | Some s -> loop (s :: acc)
      | None -> List.rev acc in
    loop []
  in
  List.fold_left (fun a e -> a ^ e) "" @@ lines

let tokenize_from_channel (c : in_channel) : token list =
  Lexer.tokenize @@ string_of_in_channel c

let tokenize_from_file (filename : string) : token list =
  let c = open_in filename in
  let s = tokenize_from_channel c in
  close_in c;
  s


(**********************************
 *        Evaluator Utils         *
 **********************************)

(* Capture printed output for local checks. *)
let (print_buffer : Buffer.t) = Buffer.create 100

let flush_print_buffer () : unit = Buffer.clear print_buffer;;
let assert_buffer_equal (s : string) : unit =
  let c = Buffer.contents print_buffer in
  if not (s = c) then failwith ("Printed: '" ^ c ^ "' Expected:'" ^ s ^ "'");;

(* Write output to both the terminal and the capture buffer. *)
let print_output_string (s : string) : unit =
  Buffer.add_string print_buffer s;
  Stdlib.print_string s

let print_output_int (i : int) : unit =
  print_output_string (string_of_int i)

let print_output_bool (b : bool) : unit =
  print_output_string (string_of_bool b)

let print_output_newline () : unit =
  print_output_string "\n"

let read_from_file (input_filename : string) : string =
  let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
      try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
      | Some s -> loop (s :: acc)
      | None -> close_in ic; List.rev acc in
    loop []
  in
  (* Read the file into lines *)
  let prog_lines = read_lines input_filename in
  (* Compress to a single string *)
  List.fold_left (fun a e -> a ^ e ^ "\n") "" prog_lines

(* AST formatting *)

let unparse_data_type = function
| Int_Type -> "int"
| Bool_Type -> "bool"

let unparse_expr = show_expr

let rec unparse_stmt (s : stmt) : string = match s with
  | NoOp -> "NoOp"
  | Seq(s1, s2) -> "Seq(" ^ unparse_stmt s1 ^ ", " ^ unparse_stmt s2 ^ ")"
  | Declare(t, id) -> "Declare(" ^ unparse_data_type t ^ ", " ^ id ^ ")"
  | Assign(id, e) -> "Assign(" ^ id ^ ", " ^ unparse_expr e ^ ")"
  | If(guard, if_body, else_body) ->
    "If(" ^ unparse_expr guard ^ ", " ^ unparse_stmt if_body ^ ", " ^ unparse_stmt else_body ^ ")"
  | While(guard, body) -> "While(" ^ unparse_expr guard ^ ", " ^ unparse_stmt body ^ ")"
  | For(id, start_expr, end_expr, body) -> "For(" ^ id ^ " from " ^ unparse_expr start_expr ^ " to " ^ unparse_expr end_expr ^ "){" ^ unparse_stmt body ^ "}"
  | Print(e) -> "Print(" ^ unparse_expr e ^ ")"
  | Return(e) -> "Return(" ^ unparse_expr e ^")"

let rec unparse_defn = function
| Multi(d, d') -> unparse_defn d ^ "\n" ^ unparse_defn d'
| Fun(rt, id, params, body) -> 
  unparse_data_type rt ^ " " ^ id ^ " " ^ "("
  ^ ListLabels.fold_right ~f:(fun (n, _) a -> ", " ^ n ^ a) params ~init:"" 
  ^ ") {\n" ^ unparse_stmt body ^ "\n}"
| Main(body) -> "int main() {\n" ^ unparse_stmt body ^ "\n}"

(* Removes shadowed bindings from an execution environment *)
let prune_env env =
  let binds = List.sort_uniq compare (List.map fst env.bindings) in
  { env with bindings=List.map (fun e -> (e, List.assoc e env.bindings)) binds }

(* Eval report print function *)

let print_eval_env_report (env : environment): unit =

  print_string "*** BEGIN POST-EXECUTION ENVIRONMENT REPORT ***\n";

  
  env |> show_environment |> print_string;

  print_string "***  END POST-EXECUTION ENVIRONMENT REPORT  ***\n"
