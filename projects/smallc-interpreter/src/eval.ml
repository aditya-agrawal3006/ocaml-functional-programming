open SmallCTypes
open Utils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

(** Evaluate an expression in [env], checking operand types and calls. *)
let rec eval_expr env t =
  match t with
    | Int n -> Int_Val n
    | Bool b -> Bool_Val b
    | ID x -> begin match List.assoc_opt x env.bindings with
              | Some v -> v
              | None -> raise (DeclareError "not found")
              end
    | Add (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Int_Val (x + y)
                      | _ -> raise (TypeError "bad type")
                      end
    | Sub (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Int_Val (x - y)
                      | _ -> raise (TypeError "bad type")
                      end
    | Mult (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Int_Val (x * y)
                      | _ -> raise (TypeError "bad type")
                      end
    | Div (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> if y = 0 
                                                then raise DivByZeroError 
                                                else Int_Val (x / y)
                      | _ -> raise (TypeError "bad type")
                      end
    | Pow (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Int_Val (int_of_float (floor (float_of_int x ** float_of_int y)))
                      | _ -> raise (TypeError "bad type")
                      end
    | And (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Bool_Val x, Bool_Val y -> Bool_Val(x && y)
                      | _ -> raise (TypeError "bad type")
                      end
    | Or (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Bool_Val x, Bool_Val y -> Bool_Val(x || y)
                      | _ -> raise (TypeError "bad type")
                      end              
    | Not e -> let v =  eval_expr env e in
                begin match v with
                | Bool_Val x -> Bool_Val (not x)
                | _ -> raise (TypeError "bad type")
                end
    | Less (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Bool_Val(x < y)
                      | _ -> raise (TypeError "bad type")
                      end
    | Greater (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Bool_Val(x > y)
                      | _ -> raise (TypeError "bad type")
                      end
    | LessEqual (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Bool_Val(x <= y)
                      | _ -> raise (TypeError "bad type")
                      end
    | GreaterEqual (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Bool_Val(x >= y)
                      | _ -> raise (TypeError "bad type")
                      end
    | Equal (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Bool_Val(x = y)
                      | Bool_Val x, Bool_Val y -> Bool_Val(x = y)
                      | _ -> raise (TypeError "bad type")
                      end
    | NotEqual (e, e2) -> let v = eval_expr env e in
                      let v2 = eval_expr env e2 in
                      begin match v, v2 with
                      | Int_Val x, Int_Val y -> Bool_Val(x <> y)
                      | Bool_Val x, Bool_Val y -> Bool_Val(x <> y)
                      | _ -> raise (TypeError "bad type")
                      end
    (* Bind evaluated arguments in a new function environment. *)
    | Call (x,args) -> begin match List.assoc_opt x env.defns with
                      | None -> raise (DeclareError "not found")
                      | Some (typ, params, body) -> if List.length args <> List.length params 
                                                    then raise (TypeError "wrong args");
                                                          let vals = List.map (eval_expr env) args in
                                                          let binds = List.map2 (fun (x, typ) v -> begin match typ, v with
                                                                                                  | Int_Type, Int_Val _
                                                                                                  | Bool_Type, Bool_Val _ -> (x, v)
                                                                                                  | _ -> raise (TypeError "wrong arg type")
                                                                                                  end
                                                                                ) params vals in

                                                          let env2 = {defns = env.defns; bindings = binds} in
                                                          let env2 = eval_stmt env2 body in
                                                          begin match List.assoc_opt "return" env2.bindings with
                                                          | None -> raise (TypeError "missing ret")
                                                          | Some v ->
                                                              begin match typ, v with
                                                              | Int_Type, Int_Val _
                                                              | Bool_Type, Bool_Val _ -> v
                                                              | _ -> raise (TypeError "wrong ret type")
                                                              end
                                                          end
                      end

(** Execute a statement and return the updated environment. *)
and eval_stmt env s =
  match s with
  | NoOp -> env
  | Seq (s1, s2) -> let env2 = eval_stmt env s1 in
                    eval_stmt env2 s2
  | Return e -> let v = eval_expr env e in
                {env with bindings = ("return", v) :: env.bindings}
  | Declare (typ, x) -> if List.mem_assoc x env.bindings 
                        then raise (DeclareError "alr dec")
                        else let v = match typ with
                                    | Int_Type -> Int_Val 0
                                    | Bool_Type -> Bool_Val false
                        in {env with bindings = (x, v) :: env.bindings}
  
  | Assign (x, e) -> let old = match List.assoc_opt x env.bindings with
                              | Some v -> v
                              | None -> raise (DeclareError "not dec")
                      in
                      let v = eval_expr env e in
                      begin match old, v with
                      | Int_Val _, Int_Val _
                      | Bool_Val _, Bool_Val _ -> let b = (x, v) :: List.remove_assoc x env.bindings in
                                                  {env with bindings = b}
                      | _ -> raise (TypeError "wrong type")
                      end

  | If (e, e2, e3) -> let v = eval_expr env e in
                      begin match v with
                      | Bool_Val true -> eval_stmt env e2
                      | Bool_Val false -> eval_stmt env e3
                      | _ -> raise (TypeError "bad if")
                      end
  (* Re-evaluate the guard after every iteration. *)
  | While (e, e2) -> let v = eval_expr env e in
                      begin match v with
                      | Bool_Val true ->
                          let env2 = eval_stmt env e2 in
                          eval_stmt env2 (While (e, e2))
                      | Bool_Val false -> env
                      | _ -> raise (TypeError "bad while")
                      end

  (* The loop variable advances from the start through the end value. *)
  | For (x, e, e2, s) -> let v = eval_expr env e in
                          let v2 = eval_expr env e2 in
                          begin match v, v2 with
                          | Int_Val a, Int_Val b -> let env2 = {env with bindings = (x, Int_Val a) :: List.remove_assoc x env.bindings} in
                              if a > b 
                              then env2
                              else let env3 = eval_stmt env2 s in
                                    let env4 = eval_stmt env3
                                    (Assign (x, Add (ID x, Int 1))) in
                                    eval_stmt env4 (For (x, ID x, Int b, s))
                          | _ -> raise (TypeError "bad for")
                          end
  | Print e -> let v = eval_expr env e in
                begin match v with
                | Int_Val x -> print_output_int x
                | Bool_Val b -> print_output_bool b
                end;
                print_output_newline ();
                env

(** Register functions and execute the program entry point. *)
let rec eval env d =
  match d with
  | Fun (typ, x, args, s) -> if List.mem_assoc x env.defns || List.mem_assoc x env.bindings 
                              then raise (DeclareError "alr def")
                              else {env with defns = (x, (typ, args, s)) :: env.defns}
  | Multi (d1, d2) -> let env2 = eval env d1 in
                      eval env2 d2
  | Main s -> eval_stmt env s
