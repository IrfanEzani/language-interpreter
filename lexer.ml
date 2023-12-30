open TokenTypes

let tokenize input = 

(* parenthesis *)
  let lparen_reg = Str.regexp "(" in
  let rparen_reg = Str.regexp ")" in

(* comparisons *)
  let comp_reg = Str.regexp "=\\|<>\\|>\\|<\\|>=\\|<=\\" in


(*booleans*)
  let or_reg = Str.regexp "||" in
  let and_reg = Str.regexp "&&" in
  let not_reg = Str.regexp "not" in
  let if_reg = Str.regexp "if" in
  let then_reg = Str.regexp "then" in
  let else_reg = Str.regexp "else" in

(*arithmetics*)
  let add_reg = Str.regexp "+" in
  let sub_reg = Str.regexp "-" in
  let mul_reg = Str.regexp "*" in
  let div_reg = Str.regexp "/" in

(*keywords*)
  let con_reg = Str.regexp "\\^" in
  let let_reg = Str.regexp "let" in
  let rec_reg = Str.regexp "rec" in
  let in_reg = Str.regexp "in" in
  let def_reg = Str.regexp "def" in
  let fun_reg = Str.regexp "fun" in
  let arw_reg = Str.regexp "->" in

(*numbers and id*)
  let pos_reg = Str.regexp "[0-9]+" in
  let neg_reg = Str.regexp "(-[0-9]+)" in

  let bool_reg = Str.regexp "true\\|false" in
  let str_reg = Str.regexp "\"\\([^\"]*\\)\"" in
  let id_reg = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let idcheck_reg = Str.regexp "[a-zA-Z0-9]+" in
  let white_reg = Str.regexp "[ \t\n]+" in
  let dubsemi_reg = Str.regexp ";;" in


(*mklst function*)
  let rec mklst pos s = 
    
    (* list complete *)
      if pos >= (String.length s) then [] 
      else
      
    (* parenthesis *)
      if Str.string_match lparen_reg s pos then Tok_LParen :: mklst (Str.match_end()) s
      else if Str.string_match rparen_reg s pos then Tok_RParen :: mklst (Str.match_end()) s

    (* comparisons *)
      else if Str.string_match comp_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = Str.match_end() in
        match tok with
        | "=" -> Tok_Equal :: mklst tok_end s
        | "<>" -> Tok_NotEqual :: mklst tok_end s
        | "<=" -> Tok_LessEqual :: mklst tok_end s
        | ">=" -> Tok_GreaterEqual :: mklst tok_end s
        | "<"  -> Tok_Less :: mklst tok_end s
        | ">"  -> Tok_Greater :: mklst tok_end s
        | _ -> raise (InvalidInputException "lexing error")


    (*booleans*)
      else if Str.string_match or_reg s pos then Tok_Or::mklst (Str.match_end()) s
 
      else if Str.string_match and_reg s pos then Tok_And::mklst (Str.match_end()) s
 
      else if Str.string_match not_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if Str.string_match idcheck_reg s tok_end then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_Not::(mklst tok_end s)

      else if Str.string_match if_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if (Str.string_match idcheck_reg s tok_end) then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_If::(mklst tok_end s)

      else if Str.string_match then_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if (Str.string_match idcheck_reg s tok_end) then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_Then::(mklst tok_end s)

      else if Str.string_match else_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if (Str.string_match idcheck_reg s tok_end) then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_Else::(mklst tok_end s)
        
    
    (*keywords*)
      else if Str.string_match con_reg s pos then Tok_Concat::(mklst (Str.match_end()) s)

      else if Str.string_match let_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if (Str.string_match idcheck_reg s tok_end) then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_Let::(mklst tok_end s)

      else if Str.string_match rec_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if (Str.string_match idcheck_reg s tok_end) then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_Rec::(mklst tok_end s)

      else if Str.string_match in_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if (Str.string_match idcheck_reg s tok_end) then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_In::(mklst tok_end s)
      
      else if Str.string_match def_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if (Str.string_match idcheck_reg s tok_end) then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_Def::(mklst tok_end s)

      else if Str.string_match fun_reg s pos then
        let tok = Str.matched_string s in
        let tok_end = (Str.match_end()) in

        if (Str.string_match idcheck_reg s tok_end) then
          let id = Str.matched_string s in 
          (Tok_ID(tok^id))::(mklst (Str.match_end()) s)
        else
          Tok_Fun::(mklst tok_end s)

      else if Str.string_match arw_reg s pos then Tok_Arrow::(mklst (Str.match_end()) s)
    
    
    (*arithmetics*)
      else if Str.string_match add_reg s pos then Tok_Add::(mklst (Str.match_end()) s)
      else if Str.string_match sub_reg s pos then Tok_Sub::(mklst (Str.match_end()) s)
      else if Str.string_match mul_reg s pos then Tok_Mult::(mklst (Str.match_end()) s)
      else if Str.string_match div_reg s pos then Tok_Div::(mklst (Str.match_end()) s)

    
    (*numbers and IDs*)
      (* integers *)
        
        else if Str.string_match pos_reg s pos then
          let curr = Str.matched_string s in
          Tok_Int(int_of_string curr)::(mklst (Str.match_end()) s)
        
        else if Str.string_match neg_reg s pos then
          let curr = (Str.matched_string s) in
          let sub = String.sub curr 1 ((String.length curr) - 2) in
          Tok_Int(int_of_string sub)::(mklst (Str.match_end()) s)
      
      (* bool *)
        else if Str.string_match bool_reg s pos then
          let curr = (Str.matched_string s) in
          Tok_Bool(bool_of_string curr)::(mklst (Str.match_end()) s)

      (*str *)
        else if Str.string_match str_reg s pos then
          let curr = (Str.matched_string s) in
          let unescaped = (Str.matched_group 1 s) in
          Tok_String(unescaped)::(mklst (pos + (String.length curr)) s)

      (*id *)
        else if Str.string_match id_reg s pos then
          let curr = (Str.matched_string s) in
          Tok_ID(curr)::(mklst (pos + (String.length curr)) s)

      (* whitespace *)
        else if Str.string_match white_reg s pos then mklst (Str.match_end()) s

      (* double semicolons *)
        else if Str.string_match dubsemi_reg s pos then Tok_DoubleSemi::(mklst (Str.match_end()) s)

    (* exception *)
      else raise (InvalidInputException "Lexing error")

  in mklst 0 input;;