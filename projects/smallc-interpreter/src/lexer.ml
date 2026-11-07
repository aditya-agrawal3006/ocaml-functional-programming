open TokenTypes

(** Split SmallC source code into tokens, ending with [EOF]. *)
let tokenize input =
  let len = String.length input in
  (** Check whether a character is a decimal digit. *)
  let digit c = (c >= '0' && c <= '9') in 
  (** Find the end of an integer literal starting at [i]. *)
  let rec readnum i =
    if i < len && digit input.[i] then
      readnum (i + 1)
    else
      i
  in 
  (** Check whether a character is an ASCII letter. *)
  let letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  in
  (** Read an identifier or keyword, allowing digits after the first letter. *)
  let rec readword i = 
    if i < len && (letter input.[i] || digit input.[i]) 
    then readword (i+1)
    else i
  in

  (** Read the next token and continue from the following character. *)
  let rec scan i =
    if i >= len 
    then [EOF]
    else if input.[i] = ' ' || input.[i] = '\n' || input.[i] = '\t' || input.[i] = '\r' then
      scan (i + 1)

    else if digit input.[i] || (input.[i] = '-' && i + 1 < len && digit input.[i+1])
    then let start = if input.[i] = '-'
                      then i + 1
                      else i 
    in 
    let j = readnum start in
    let num = String.sub input i (j-i) in
    Tok_Int (int_of_string num) :: (scan j)

    else if letter input.[i] then
      let j = readword i in
      let word = String.sub input i (j - i) in
      let tok = match word with
        | "int" -> Tok_Int_Type
        | "bool" -> Tok_Bool_Type
        | "printf" -> Tok_Print
        | "main" -> Tok_Main
        | "if" -> Tok_If
        | "else" -> Tok_Else
        | "while" -> Tok_While
        | "for" -> Tok_For
        | "from" -> Tok_From
        | "to" -> Tok_To
        | "return" -> Tok_Return
        | "true" -> Tok_Bool true
        | "false" -> Tok_Bool false
        | _ -> Tok_ID word
      in tok :: scan j
    
    else if i + 1 < len && String.sub input i 2 = "==" 
    then Tok_Equal :: scan (i + 2)

    else if i + 1 < len && String.sub input i 2 = "!="
    then Tok_NotEqual :: scan (i + 2)

    else if i + 1 < len && String.sub input i 2 = ">=" 
    then Tok_GreaterEqual :: scan (i + 2)
    
    else if i + 1 < len && String.sub input i 2 = "<=" 
    then Tok_LessEqual :: scan (i + 2)

    else if i + 1 < len && String.sub input i 2 = "||" 
    then Tok_Or :: scan (i + 2)

    else if i + 1 < len && String.sub input i 2 = "&&" 
    then Tok_And :: scan (i + 2)

    else match input.[i] with
      | '(' -> Tok_LParen :: scan (i + 1)
      | ')' -> Tok_RParen :: scan (i + 1)
      | '{' -> Tok_LBrace :: scan (i + 1)
      | '}' -> Tok_RBrace :: scan (i + 1)
      | '=' -> Tok_Assign :: scan (i + 1)
      | '>' -> Tok_Greater :: scan (i + 1)
      | '<' -> Tok_Less :: scan (i + 1)
      | '!' -> Tok_Not :: scan (i + 1)
      | ';' -> Tok_Semi :: scan (i + 1)
      | ',' -> Tok_Comma :: scan (i + 1)
      | '+' -> Tok_Add :: scan (i + 1)
      | '-' -> Tok_Sub :: scan (i + 1)
      | '*' -> Tok_Mult :: scan (i + 1)
      | '/' -> Tok_Div :: scan (i + 1)
      | '^' -> Tok_Pow :: scan (i + 1)
      | _ -> raise (InvalidInputException "bad token")



  in scan 0
