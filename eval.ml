open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = match e with
  | Int x -> Int x
  | Bool x -> Bool x
  | String x -> String x
  | ID x -> (*Check this*)
    let (found, value) = List.fold_left (fun (found, value) (var, pair) -> 
      if (x = var) then 
        (true, !pair) 
      else
        (found, value)) (false, ID(x)) env in
    if (found) then
      value
    else
      raise (DeclareError ("Did not declare variable"))
  | Not x -> 
    (let x' = eval_expr env x in
    match x' with
    | Bool true -> Bool false
    | Bool false -> Bool true
    | _ -> raise (TypeError "Expected type bool"))
  | Binop (op, e1, e2) -> 
    (let e1' = eval_expr env e1 in
    let e2' = eval_expr env e2 in
    match e1', e2' with 
    | Int x, Int y -> 
      (match op with
      | Add -> Int (x + y)
      | Sub -> Int (x - y)
      | Mult -> Int (x * y)
      | Div -> 
        if (y == 0) then
          raise (DivByZeroError)
        else
          Int (x / y)
      | Greater -> Bool (x > y)
      | Less -> Bool (x < y)
      | GreaterEqual -> Bool (x >= y)
      | LessEqual -> Bool (x <= y)
      | Equal -> Bool (x == y)
      | NotEqual -> Bool (x <> y)
      | _ -> raise (TypeError "op not used for string/bool except equal/notequal"))
    | String x, String y -> 
      (match op with
      | Concat -> String (x ^ y)
      | Equal -> if (String.compare x y == 0) then Bool true else Bool false
      | NotEqual -> if (String.compare x y <> 0) then Bool true else Bool false
      | _ -> raise (TypeError "op not used for int/bool except equal/notequal"))
    | Bool x, Bool y -> 
        (match op with
        | Or -> Bool (x || y)
        | And -> Bool (x && y)
        | Equal -> if (x && y) then Bool true else Bool false
        | NotEqual -> if (x && y) then Bool false else Bool true
        | _ -> raise (TypeError "op not used for string/int except equal/notequal"))
    | _ -> raise (TypeError "Not the same types"))
  | If (guard, true_branch, false_branch) -> 
    (let guard' = eval_expr env guard in
    match guard' with
      | Bool x -> 
        if (x) then
          eval_expr env true_branch
        else 
          eval_expr env false_branch
      | _ -> raise (TypeError "not a bool for if"))
  | Let (var, rec_var, int_expr, body_expr) -> 
    if rec_var then
      let env' = extend_tmp env var in
      let initial = eval_expr env' int_expr in
      update env' var initial; eval_expr env' body_expr
    else 
      let int_expr' = eval_expr env int_expr in
      let env' = extend env var int_expr' in
      eval_expr env' body_expr
  | Fun (id_name, body_expr) -> 
     Closure (env, id_name, body_expr)
  | App (expr1, expr2) ->
    (let expr1_closure = eval_expr env expr1 in
    match expr1_closure with
      | Closure (env', id_name, body_expr) ->  
      let expr_v = eval_expr env expr2 in
      let env'' = extend env' id_name expr_v in 
      eval_expr env'' body_expr
      | _ -> raise (TypeError "App expr1 not a closure"))
  | Record (expr) -> e
  | Select (label, expr) -> 
    (let record_expr = eval_expr env expr in 
    match record_expr with 
    | Record (expr) ->
      let (found, value) = List.fold_left (fun (found, value) (label_record, expr_record) -> 
        if (label_record = label) then 
          (true, expr_record) 
        else
          (found, value)) (false, Record(expr)) expr in
      if (found) then
        eval_expr env value
      else
        raise (SelectError "No label found in record for Select")
    | _ -> raise (TypeError "Select expr not a record"))
  | _ -> raise (InvalidInputException "not a valid expr")

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
  | Def (var, expr1) -> 
    let env' = extend_tmp env var in
    let ex = eval_expr env' expr1 in
    update env' var ex;
    (env', Some ex)
  | Expr expr1 -> 
    let result = eval_expr env expr1 in
    (env, Some result)
  | NoOp -> (env, None)
  | _ -> raise (InvalidInputException "not a valid mutop")
   
