open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

(*Make sure to compare certain conditions first before
   others. (Ex: <> before < and >)*)
let re_space = Str.regexp "[ \t\n]"
let re_int = Str.regexp "[0-9]+\\|(-[0-9]+)"
let re_Lparen = Str.regexp "("
let re_Rparen = Str.regexp ")"
let re_Lcurly = Str.regexp "{"
let re_Rcurly = Str.regexp "}"
let re_dot = Str.regexp "\\."
let re_notEqual = Str.regexp "<>"
let re_greaterEqual = Str.regexp ">="
let re_lessEqual = Str.regexp "<="
let re_equal = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "\\(not\\$\\|not \\)"
let re_if = Str.regexp "\\(if\\$\\|if \\)"
let re_then = Str.regexp "\\(then\\$\\|then \\)"
let re_else = Str.regexp "\\(else\\$\\|else \\)"
let re_add = Str.regexp "\\+"
let re_arrow = Str.regexp "->"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "\\*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "\\(let\\$\\|let \\)"
let re_def = Str.regexp "\\(def\\$\\|def \\)"
let re_in = Str.regexp "\\(in\\$\\|in \\)"
let re_rec = Str.regexp "\\(rec\\$\\|rec \\)"
let re_fun = Str.regexp "\\(fun\\$\\|fun \\)"
let re_doubleSemi = Str.regexp ";;"
let re_semi = Str.regexp ";"
let re_bool = Str.regexp "true\\|false"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_string = Str.regexp "\"[^\"]*\""


let tokenize input = 
  let rec tok pos s =

  if pos >= String.length s then 
    []
  else 
    if Str.string_match re_int s pos then
      let token = Str.matched_string s in
      let len = String.length token in
      let check_parens s =
        let left = 
          try String.rindex s '(' with Not_found -> -1
        in
        let right =
          try String.index s ')' with Not_found -> String.length s
        in
      String.sub s (left + 1) (right - left - 1)
      in
    
      Tok_Int (int_of_string (check_parens token)) :: tok (pos + len) s

    else if Str.string_match re_Lparen s pos then
      Tok_LParen :: tok (pos+1) s

    else if Str.string_match re_Rparen s pos then
      Tok_RParen :: tok (pos+1) s

    else if Str.string_match re_Lcurly s pos then
      Tok_LCurly :: tok (pos+1) s

    else if Str.string_match re_Rcurly s pos then
      Tok_RCurly :: tok (pos+1) s

    else if Str.string_match re_dot s pos then
      Tok_Dot :: tok (pos+1) s

    else if Str.string_match re_notEqual s pos then
      Tok_NotEqual :: tok (pos+2) s

    else if Str.string_match re_greaterEqual s pos then
      Tok_GreaterEqual :: tok (pos+2) s

    else if Str.string_match re_lessEqual s pos then
      Tok_LessEqual :: tok (pos+2) s

    else if Str.string_match re_equal s pos then
      Tok_Equal :: tok (pos+1) s

    else if Str.string_match re_greater s pos then
      Tok_Greater :: tok (pos+1) s

    else if Str.string_match re_less s pos then
      Tok_Less :: tok (pos+1) s

    else if Str.string_match re_or s pos then
      Tok_Or :: tok (pos+2) s

    else if Str.string_match re_and s pos then
      Tok_And :: tok (pos+3) s

    else if Str.string_match re_not s pos then
      Tok_Not :: tok (pos+3) s

    else if Str.string_match re_if s pos then
      Tok_If :: tok (pos+2) s

    else if Str.string_match re_then s pos then
      Tok_Then :: tok (pos+4) s

    else if Str.string_match re_else s pos then
      Tok_Else :: tok (pos+4) s

    else if Str.string_match re_add s pos then
      Tok_Add :: tok (pos+1) s

    else if Str.string_match re_arrow s pos then
      Tok_Arrow :: tok (pos+2) s

    else if Str.string_match re_sub s pos then
      Tok_Sub :: tok (pos+1) s

    else if Str.string_match re_mult s pos then
      Tok_Mult :: tok (pos+1) s

    else if Str.string_match re_div s pos then
      Tok_Div :: tok (pos+1) s

    else if Str.string_match re_concat s pos then
      Tok_Concat :: tok (pos+1) s

    else if Str.string_match re_let s pos then
      Tok_Let :: tok (pos+3) s

    else if Str.string_match re_def s pos then
      Tok_Def :: tok (pos+3) s

    else if Str.string_match re_in s pos then
      Tok_In :: tok (pos+2) s

    else if Str.string_match re_rec s pos then
      Tok_Rec :: tok (pos+3) s

    else if Str.string_match re_fun s pos then
      Tok_Fun :: tok (pos+3) s

    else if Str.string_match re_doubleSemi s pos then
      Tok_DoubleSemi :: tok (pos+2) s

    else if Str.string_match re_semi s pos then
      Tok_Semi :: tok (pos+1) s

    else if Str.string_match re_bool s pos then
      let token = Str.matched_string s in
      let len = String.length token in
      Tok_Bool (bool_of_string token) :: tok (pos + len) s

    else if Str.string_match re_id s pos then
      let token = Str.matched_string s in
      let len = String.length token in
      Tok_ID token :: tok (pos + len) s

    else if Str.string_match re_string s pos then
      let token = Str.matched_string s in
      let len = String.length token in
      let sanitize s = String.sub s 1 ((String.length s) - 2)
      in
      Tok_String (sanitize token) :: tok (pos + len) s

    else if Str.string_match re_space s pos then
      let token = Str.matched_string s in
      let len = String.length token in
      tok (pos + len) s

    else raise (InvalidInputException "tokenize")
  in
tok 0 input