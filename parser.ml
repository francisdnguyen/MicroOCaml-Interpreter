open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  if (lookahead toks = Some Tok_Let) then
    parse_let (match_token toks Tok_Let)

  else if (lookahead toks = Some Tok_If) then
    parse_if toks

  else if (lookahead toks = Some Tok_Fun) then 
    parse_fun toks

  else
    parse_or toks

  and parse_let toks = match lookahead toks with
    | Some Tok_Rec -> (let t_id_list = match_token toks Tok_Rec in match lookahead t_id_list with
      | Some Tok_ID x -> (let t_equal_list = match_token t_id_list (Tok_ID x) in match lookahead t_equal_list with
        | Some Tok_Equal -> 
          (let t_expr1_list = match_token t_equal_list (Tok_Equal) in
          let (t1, e1) = parse_expr t_expr1_list in
            match lookahead t1 with
            | Some Tok_In -> 
              let t_expr2_list = match_token t1 (Tok_In) in
              let (t2, e2) = parse_expr t_expr2_list in
              (t2, Let (x, true, e1, e2))
            | _ -> raise (InvalidInputException "parse_let_in"))
        | _ -> raise (InvalidInputException  "parse_let_equal"))
      | _ -> raise (InvalidInputException  "parse_let_id"))
    | Some Tok_ID x -> (let t_equal_list = match_token toks (Tok_ID x) in match lookahead t_equal_list with
      | Some Tok_Equal -> 
        (let t_expr1_list = match_token t_equal_list (Tok_Equal) in
        let (t1, e1) = parse_expr t_expr1_list in
          match lookahead t1 with
          | Some Tok_In -> 
            let t_expr2_list = match_token t1 (Tok_In) in
            let (t2, e2) = parse_expr t_expr2_list in
            (t2, Let (x, false, e1, e2))
          | _ -> raise (InvalidInputException "parse_let_in"))
      | _ -> raise (InvalidInputException  "parse_let_equal"))
    | _ -> raise (InvalidInputException "parse_let")
    
  and parse_if toks = match lookahead toks with
    | Some Tok_If ->
      let t_expr1_list = match_token toks (Tok_If) in
      let (t1, e1) = parse_expr t_expr1_list in
      (match lookahead t1 with
      | Some Tok_Then ->
        let t_expr2_list = match_token t1 (Tok_Then) in
        let (t2, e2) = parse_expr t_expr2_list in
        (match lookahead t2 with
        | Some Tok_Else ->
          let t_expr3_list = match_token t2 (Tok_Else) in
          let (t3, e3) = parse_expr t_expr3_list in
          (t3, If (e1, e2, e3))
        | _ -> raise (InvalidInputException "parse_if_else"))
      | _ -> raise (InvalidInputException "parse_if_then"))
    | _ -> raise (InvalidInputException "parse_if")

  and parse_fun toks = match lookahead toks with
    | Some Tok_Fun -> (let t_id_list = match_token toks (Tok_Fun) in match lookahead t_id_list with
      | Some Tok_ID x -> (let t_arrow_list = match_token t_id_list (Tok_ID x) in match lookahead t_arrow_list with
        | Some Tok_Arrow -> 
          let t_expr1_list = match_token t_arrow_list (Tok_Arrow) in
          let (t1, e1) = parse_expr t_expr1_list in
          (t1, Fun (x, e1))
        | _ -> raise (InvalidInputException "parse_fun_arrow"))
      | _ -> raise (InvalidInputException "parse_fun_id"))
    | _ -> raise (InvalidInputException "parse_fun")
 
    and parse_or toks =
      let (t1, e1) = parse_and toks in
      match lookahead t1 with
        | Some Tok_Or -> 
          let t_or_list = match_token t1 (Tok_Or) in
          let (t2, e2) = parse_or t_or_list in
          (t2, Binop (Or, e1, e2))
        | _ -> (t1, e1)

    and parse_and toks =
      let (t1, e1) = parse_equality toks in
      match lookahead t1 with
       | Some Tok_And -> 
         let t_and_list = match_token t1 (Tok_And) in
         let (t2, e2) = parse_and t_and_list in
         (t2, Binop (And, e1, e2))
       | _ -> (t1, e1)

    and parse_equality toks =
      let (t1, e1) = parse_relational toks in
      match lookahead t1 with
       | Some Tok_Equal -> 
         let t_equal_list = match_token t1 (Tok_Equal) in
         let (t2, e2) = parse_equality t_equal_list in
         (t2, Binop (Equal, e1, e2))
       | Some Tok_NotEqual -> 
         let t_notequal_list = match_token t1 (Tok_NotEqual) in
         let (t2, e2) = parse_equality t_notequal_list in
        (t2, Binop (NotEqual, e1, e2))
       | _ -> (t1, e1)

    and parse_relational toks =
      let (t1, e1) = parse_additive toks in
      match lookahead t1 with
        | Some Tok_Less ->
          let t_less_list = match_token t1 (Tok_Less) in
          let (t2, e2) = parse_relational t_less_list in
          (t2, Binop (Less, e1, e2))
        | Some Tok_Greater ->
          let t_greater_list = match_token t1 (Tok_Greater) in
          let (t2, e2) = parse_relational t_greater_list in
          (t2, Binop (Greater, e1, e2))
        | Some Tok_LessEqual ->
          let t_lessequal_list = match_token t1 (Tok_LessEqual) in
          let (t2, e2) = parse_relational t_lessequal_list in
          (t2, Binop (LessEqual, e1, e2))
        | Some Tok_GreaterEqual ->
          let t_greaterequal_list = match_token t1 (Tok_GreaterEqual) in
          let (t2, e2) = parse_relational t_greaterequal_list in
          (t2, Binop (GreaterEqual, e1, e2))
        | _  -> (t1, e1)

    and parse_additive toks =
      let (t1, e1) = parse_multiplicative toks in
      match lookahead t1 with
        | Some Tok_Add ->
          let t_add_list = match_token t1 (Tok_Add) in
          let (t2, e2) = parse_additive t_add_list in
          (t2, Binop (Add, e1, e2))
        | Some Tok_Sub ->
          let t_sub_list = match_token t1 (Tok_Sub) in
          let (t2, e2) = parse_additive t_sub_list in
          (t2, Binop (Sub, e1, e2))
        | _  -> (t1, e1)
      
      and parse_multiplicative toks =
        let (t1, e1) = parse_concat toks in
        match lookahead t1 with
          | Some Tok_Mult ->
            let t_mult_list = match_token t1 (Tok_Mult) in
            let (t2, e2) = parse_multiplicative t_mult_list in
            (t2, Binop (Mult, e1, e2))
          | Some Tok_Div ->
            let t_div_list = match_token t1 (Tok_Div) in
            let (t2, e2) = parse_multiplicative t_div_list in
            (t2, Binop (Div, e1, e2))
          | _  -> (t1, e1)
      
      and parse_concat toks =
        let (t1, e1) = parse_unary toks in
        match lookahead t1 with
          | Some Tok_Concat -> 
            let t_concat_list = match_token t1 (Tok_Concat) in
            let (t2, e2) = parse_concat t_concat_list in
            (t2, Binop (Concat, e1, e2))
          | _ -> (t1, e1)

      and parse_unary toks = match lookahead toks with
        | Some Tok_Not -> 
          let t_not_list = match_token toks (Tok_Not) in
          let (t1, e1) = parse_unary t_not_list in
          (t1, Not (e1))
        | _ -> parse_app toks

      and parse_app toks =
        let (t1, e1) = parse_select toks in
        match lookahead t1 with
        | Some Tok_Int x -> 
          let (t2, e2) = parse_primary t1 in
          (t2, App (e1, e2))
        | Some Tok_Bool x ->
          let (t2, e2) = parse_primary t1 in
          (t2, App (e1, e2))
        | Some Tok_String x ->
          let (t2, e2) = parse_primary t1 in
          (t2, App (e1, e2))
        | Some Tok_ID x ->
          let (t2, e2) = parse_primary t1 in
          (t2, App (e1, e2))
        | Some Tok_LParen ->
          let (t2, e2) = parse_primary t1 in
          (t2, App (e1, e2))
        | Some Tok_LCurly ->
          let (t2, e2) = parse_primary t1 in
          (t2, App (e1, e2))
        | _ -> (t1, e1)

      and parse_select toks =
        let (t1, e1) = parse_primary toks in
        match lookahead t1 with
          | Some Tok_Dot -> (let t_id_list = match_token t1 Tok_Dot in match lookahead t_id_list with
            | Some Tok_ID x -> ((match_token t_id_list (Tok_ID x)), Select (Lab x, e1)) (*fix this*)
            | _ -> (t1, e1))
          | _ -> (t1, e1)

      and parse_primary toks = match lookahead toks with
        | Some Tok_Int x -> ((match_token toks (Tok_Int x)), Int x)
        | Some Tok_Bool x -> ((match_token toks (Tok_Bool x)), Bool x)
        | Some Tok_String x -> ((match_token toks (Tok_String x)), String x)
        | Some Tok_ID x -> ((match_token toks (Tok_ID x)), ID x)
        | Some Tok_LParen ->
          (let t_lparen_list = match_token toks Tok_LParen in
          let (t1, e1) = parse_expr t_lparen_list in
          match lookahead t1 with
            | Some Tok_RParen -> (match_token t1 Tok_RParen, e1)
            | _ -> raise (InvalidInputException "parse_rparen"))
        | _ -> parse_record toks


      and parse_record toks = match lookahead toks with
        | Some Tok_LCurly -> 
          let t_lcurly_list = match_token toks Tok_LCurly in
          let (t1, e1) = parse_recordbody t_lcurly_list in
          (t1, Record (e1))
        | _ -> raise (InvalidInputException "no option for parse_or")

      and parse_recordbody toks = match lookahead toks with
        | Some Tok_ID x -> (let t_equal_list = match_token toks (Tok_ID x) in match lookahead t_equal_list with
          | Some Tok_Equal -> 
            (let t_parse_list = match_token t_equal_list (Tok_Equal) in
            let (t1, e1) = parse_expr t_parse_list in
            match lookahead t1 with
              | Some Tok_Semi -> 
                let t_recordbody_list = match_token t1 (Tok_Semi) in
                let (t2, e2) = parse_recordbody t_recordbody_list in 
                t2, [(Lab x, e1)] @ e2
              | Some Tok_RCurly -> match_token t1 (Tok_RCurly), [(Lab x, e1)]
              | _ -> raise (InvalidInputException "parse_recordbody_rcurly"))
          | _ -> raise (InvalidInputException "parse_recordbody_equal"))
        | Some Tok_RCurly -> ([], [])
        | _ -> raise (InvalidInputException "parse_rcurly_base")
(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  if (lookahead toks = Some Tok_Def) then
    parse_def (match_token toks Tok_Def)


  else if (lookahead toks = Some Tok_DoubleSemi) then 
    (match_token toks Tok_DoubleSemi, NoOp)

  else
    parse_expr_mutop toks
  
  and parse_def toks = match lookahead toks with
    | Some Tok_ID x -> (let t_equal_list = match_token toks (Tok_ID x) in match lookahead t_equal_list with
      | Some Tok_Equal ->
        (let (t1, e1) = parse_expr (match_token t_equal_list Tok_Equal) in
        match lookahead t1 with
        | Some Tok_DoubleSemi -> (match_token t1 Tok_DoubleSemi, Def (x, e1))
        | _ -> raise (InvalidInputException "parse_expr_doublesemi"))
      | _ -> raise (InvalidInputException "parse_def_equal"))
    | _ -> raise (InvalidInputException "parse_def")

  and parse_expr_mutop toks =
    let (t1, e1) = parse_expr toks in
    match lookahead t1 with
      | Some Tok_DoubleSemi -> (match_token t1 Tok_DoubleSemi, Expr e1)
      | _ -> raise (InvalidInputException "parse_expr_mutop")

