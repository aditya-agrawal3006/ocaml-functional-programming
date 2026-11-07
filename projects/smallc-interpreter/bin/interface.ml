open Smallc_lang.Utils
open Smallc_lang.Lexer
open Smallc_lang.Parser
open Smallc_lang.Eval
open Smallc_lang.SmallCTypes

type run_context = {filename : string option; print_report : bool; unparse : bool };;

let print_usage () =
  print_string "\nSmallC interpreter: tokenize, parse, or run a source file.\n\n";
  print_string "Usage:\n";
  print_string "\tdune exec bin/interface.exe <mode> <filename>: Read source code from a file (lex/parse can also use standard input).\n";
  print_string "Modes:\n";
  print_string "\tlex: Run in lex mode to show the tokens from the lexer\n";
  print_string "\tparse: Parse a full program, including function definitions and main\n";
  print_string "\teval: Evaluate the program\n\n";
  print_string "\t\teval Flags:\n";
  print_string "\t\t--unparse / -U : Print the parsed statement tree instead of executing the program\n";
  print_string "\t\t--report / -R : Print the final execution environment\n\n";
;;


let parse_from_string (input : string) : defn =
  (* Parse the tokens *)
  input |> tokenize |> parse_defs |> snd;;

let parse_expr_from_string es =
  es |> tokenize |> parse_expr |> snd;;
  
let parse_stmt_from_string s =
  s |> tokenize |> parse_stmt |> snd;;

(* Open file, read, lex, parse *)
let parse_from_file (input_filename : string) : defn =
  
  input_filename |> read_from_file |> parse_from_string;;

if Array.length Sys.argv < 2 then begin print_usage (); exit 1 end;;


(* Parse optional evaluation flags and the input filename. *)
let do_eval args = 
  let ctxt = List.fold_left (fun {filename = f; print_report = r; unparse = u} -> function
      | "--unparse" | "-U" -> {filename = f; print_report = r; unparse = true}
      | "--report" | "-R" -> {filename = f; print_report = true; unparse = u}
      | x -> {filename = Some(x); print_report = r; unparse = u})
      {filename = None; print_report = false; unparse = false} args in
  match ctxt.filename with
  | None -> print_usage ()
  | Some(filename) ->
    let ast = parse_from_file filename in
    if ctxt.unparse then begin
      print_string @@ unparse_defn ast;
    end else
      let final_env = eval {defns=[]; bindings=[]} ast in
      if ctxt.print_report then begin print_eval_env_report final_env; print_newline () end
  ;;

(* Dispatch to the requested mode. *)
let args = match Array.to_list Sys.argv with
  | _::"lex"::_ -> let ch = if Array.length Sys.argv > 2 then open_in Sys.argv.(2) else stdin in
                  let toks = tokenize_from_channel ch in
                    print_string @@ string_of_list ~newline:true string_of_token toks;
                  if ch != stdin then close_in ch
  | _::"parse"::_ -> let ch = if Array.length Sys.argv > 2 then open_in Sys.argv.(2) else stdin in
                    let toks = tokenize_from_channel ch in
                    (print_string @@ string_of_defn @@ snd @@ Smallc_lang.Parser.parse_defs toks;
                    print_newline ());
                  if ch != stdin then close_in ch
  | _::"eval"::t -> do_eval t
  | _ -> print_usage ()
;;
