open SmallCTypes
open Utils
open TokenTypes

(* Token stream helpers *)

(** Peek at the next token without consuming it. *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No toks")
  | h::_ -> h

(** Consume the expected token or report a syntax error. *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Recursive-descent expression and statement parsing *)

(** Parse an expression, starting with the lowest-precedence operator. *)
let rec parse_expr toks : token list * expr = por toks
(** Parse an integer, boolean, variable, or parenthesized expression. *)
and pbase toks =
  match toks with
  | Tok_Int n :: rest -> (rest, Int n)
  | Tok_Bool b :: rest -> (rest, Bool b)
  | Tok_ID x :: rest -> (rest, ID x)
  | Tok_LParen :: rest -> let (rest, e) = parse_expr rest in
                          let rest = match_token rest Tok_RParen in
                          (rest, e)
  | _ -> raise (InvalidInputException "bad exp")

(** Parse a function call and its arguments, or a basic expression. *)
and pcall toks = 
  match toks with
  | Tok_ID x :: Tok_LParen :: rest ->
                                      (** Collect comma-separated call arguments in source order. *)
                                      let rec getargs toks = 
                                        let (rest, e) = parse_expr toks in
                                        match rest with
                                        | Tok_Comma :: t -> let (t, e2) = getargs t in
                                                            (t, e :: e2)
                                        | _ -> (rest, [e])
                                      in
                                      let (rest, args) =
                                        match rest with
                                        | Tok_RParen :: _ -> (rest, [])
                                        | _ -> getargs rest
                                      in
                                      let rest = match_token rest Tok_RParen
                                      in (rest, Call (x, args))
  | _ -> pbase toks

(** Parse logical negation before the remaining operators. *)
and pnot toks =
  match toks with
  | Tok_Not :: rest -> let (rest,e) = pnot rest in (rest, Not e)
  | _ -> pcall toks

(** Parse exponentiation with right-associative grouping. *)
and ppow toks = 
  let (rest, e) = pnot toks in
  match rest with
  | Tok_Pow :: t -> let (t, e2) = ppow t in
                    (t, Pow (e,e2))
  | _ -> (rest,e)

(** Parse multiplication and division. *)
and pmult toks = 
  let (rest, e) = ppow toks in
  match rest with
  | Tok_Mult :: t -> let (t, e2) = pmult t in
                    (t, Mult (e,e2))
  | Tok_Div :: t -> let (t, e2) = pmult t in
                    (t, Div (e,e2))
  | _ -> (rest,e)

(** Parse addition and subtraction. *)
and padd toks = 
  let (rest, e) = pmult toks in
  match rest with
  | Tok_Add :: t -> let (t, e2) = padd t in
                    (t, Add (e,e2))
  | Tok_Sub :: t -> let (t, e2) = padd t in
                    (t, Sub (e,e2))
  | _ -> (rest,e)

(** Parse relational comparisons. *)
and prel toks = 
  let (rest, e) = padd toks in
  match rest with
  | Tok_Less :: t -> let (t, e2) = prel t in
                      (t, Less (e, e2))
  | Tok_Greater :: t -> let (t, e2) = prel t in
                        (t, Greater (e, e2))
  | Tok_LessEqual :: t -> let (t, e2) = prel t in
                          (t, LessEqual (e, e2))
  | Tok_GreaterEqual :: t -> let (t, e2) = prel t in
                              (t, GreaterEqual (e, e2))
  | _ -> (rest, e)

(** Parse equality and inequality checks. *)
and peq toks = 
  let (rest, e) = prel toks in
  match rest with
  | Tok_Equal :: t -> let (t, e2) = peq t in
                      (t, Equal (e, e2))
  | Tok_NotEqual :: t -> let (t, e2) = peq t in
                        (t, NotEqual (e, e2))
  | _ -> (rest, e)

(** Parse logical AND expressions. *)
and pand toks = 
  let (rest, e) = peq toks in
  match rest with
  | Tok_And :: t -> let (t, e2) = pand t in
                      (t, And (e, e2))
  | _ -> (rest, e)

(** Parse logical OR expressions. *)
and por toks = 
  let (rest, e) = pand toks in
  match rest with
  | Tok_Or :: t -> let (t, e2) = por t in
                      (t, Or (e, e2))
  | _ -> (rest, e)


(** Parse a statement block ending in a return expression. *)
and parse_stmt toks : token list * stmt =
  (** Read statements in order until a return or closing brace. *)
  let rec pcomp toks = 
    match toks with
    | Tok_Return :: _ -> (toks, NoOp)
    | Tok_RBrace:: _ -> (toks, NoOp)
    | _ -> let (rest, s) = pcomp1 toks in
            let (rest, s2) = pcomp rest in
            (rest, Seq (s,s2))
  
  (** Parse one declaration, assignment, print, or control-flow statement. *)
  and pcomp1 toks =
    match toks with
    | Tok_Int_Type :: Tok_ID x :: Tok_Semi :: t -> (t, Declare (Int_Type, x))
    | Tok_Bool_Type :: Tok_ID x :: Tok_Semi :: t -> (t, Declare (Bool_Type, x))
    | Tok_ID x :: Tok_Assign :: t -> let (t,e) = parse_expr t in
                                      let t = match_token t Tok_Semi in
                                      (t, Assign (x,e))
    | Tok_Print:: Tok_LParen :: t -> let (t,e) = parse_expr t in
                                      let t = match_token t Tok_RParen in
                                      let t = match_token t Tok_Semi in
                                      (t, Print e)
    | Tok_If :: t -> let t = match_token t Tok_LParen in
                      let (t,e) = parse_expr t in
                      let t = match_token t Tok_RParen in
                      let t = match_token t Tok_LBrace in
                      let (t,e2) = pcomp t in
                      let t = match_token t Tok_RBrace in
                      let (t, e3) = 
                        match t with
                        | Tok_Else :: t -> let t = match_token t Tok_LBrace in
                                            let (t, e3) = pcomp t in
                                            let t = match_token t Tok_RBrace in
                                            (t, e3)
                        | _ -> (t, NoOp)
                      in
                      (t, If (e, e2, e3))
    | Tok_While :: t -> let t = match_token t Tok_LParen in
                        let (t,e) = parse_expr t in
                        let t = match_token t Tok_RParen in
                        let t = match_token t Tok_LBrace in
                        let (t,e2) = pcomp t in
                        let t = match_token t Tok_RBrace in
                        (t, While (e, e2))
    | Tok_For :: t -> let t = match_token t Tok_LParen in
                        begin match t with
                        | Tok_ID x :: t -> let t = match_token t Tok_From in
                                            let (t, e) = parse_expr t in
                                            let t = match_token t Tok_To in
                                            let (t, e2) = parse_expr t in
                                            let t = match_token t Tok_RParen in
                                            let t = match_token t Tok_LBrace in
                                            let (t, e3) = pcomp t in
                                            let t = match_token t Tok_RBrace in
                                            (t, For (x, e, e2, e3))
                        | _ -> raise (InvalidInputException "bad for")
                        end
    | _ -> raise (InvalidInputException "bad stmt")
  in
  let (t, e) = pcomp toks in
  let t = match_token t Tok_Return in
  let (t, e2) = parse_expr t in
  let t = match_token t Tok_Semi in
  (t, Seq (e, Return e2))


(** Parse an integer or boolean type annotation. *)
let ptype toks =
  match toks with
  | Tok_Int_Type :: t -> (t, Int_Type)
  | Tok_Bool_Type :: t -> (t, Bool_Type)
  | _ -> raise (InvalidInputException "bad type")


(** Read function parameters and reject duplicate parameter names. *)
let rec pparams toks =
  match toks with
  | Tok_RParen :: _ -> (toks, [])
  | _ -> let (t, typ) = ptype toks in
          match t with
          | Tok_ID x :: t -> begin match t with
                              | Tok_Comma :: t -> let (t, args) = pparams t in
                                                  if args = [] || List.mem_assoc x args 
                                                  then raise (InvalidInputException "bad params")
                                                  else (t, (x, typ) :: args)
                              | _ -> (t, [(x, typ)])
                              end
          | _ -> raise (InvalidInputException "bad params")


(** Parse function definitions followed by the main entry point. *)
let rec parse_defs toks : token list * defn =
  (** Collect functions, checking names, until the main definition. *)
  let rec pdefs toks names = 
    match toks with
    | Tok_Int_Type:: Tok_Main :: t -> let t = match_token t Tok_LParen in
                                      let t = match_token t Tok_RParen in
                                      let t = match_token t Tok_LBrace in
                                      let (t, e) = parse_stmt t in
                                      let t = match_token t Tok_RBrace in
                                      begin match t with
                                      | [EOF] -> ([], Main e)
                                      | _ -> raise (InvalidInputException "bad")
                                      end
    | Tok_Int_Type :: _ | Tok_Bool_Type :: _ -> let (t, typ) = ptype toks in
                                                begin match t with
                                                | Tok_ID x :: t -> if List.mem x names 
                                                                    then raise (InvalidInputException "dup");
                                                                    let t = match_token t Tok_LParen in
                                                                    let (t, args) = pparams t in
                                                                    let t = match_token t Tok_RParen in
                                                                    let t = match_token t Tok_LBrace in
                                                                    let (t, e) = parse_stmt t in
                                                                    let t = match_token t Tok_RBrace in
                                                                    let (t, e2) = pdefs t (x :: names) in
                                                                    (t, Multi (Fun (typ, x, args, e), e2))

                                                | _ -> raise (InvalidInputException "bad")
                                                end
    | _ -> raise (InvalidInputException "bad")
  in pdefs toks []
