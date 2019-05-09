(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)
    
open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime, generated by an explicit "raise" construct *)
exception EvalException ;;


(*......................................................................
  Environments and values 
 *)

module type Env_type =
  sig
    type env
    type value =
      | Val of expr
      | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp : expr) (env : env) : value =
      Closure (exp, env) ;;

    (* Looks up the value of a variable in the environment *)
    let lookup (env : env) (varname : varid) : value =
      let _, value = List.find (fun v -> fst v = varname) env in
      !value ;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      (varname, loc) :: env ;;

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
      let value_to_string ?(printenvp : bool = true) (v : value) : string =
        failwith "not yet implemented"
(*    let value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val x -> "Val " ^ (exp_to_concrete_string x)
      | Closure (x, env) ->
          if ~(printenvp)
            then "Closure (" ^ (exp_to_concrete_string x) ^ ", " ^
            env_to_string env ^ ")"
          else "Val " ^ (exp_to_concrete_string x) ;;
*)
    (* Returns a printable string representation of an environment *)
      let env_to_string (env : env) : string =
        failwith "not yet implemented"
(*    let env_to_string (env : env) : string =
      List.fold_left (fun x -> ^ "x") "" env ;;
*)
  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below, evaluates an expression exp
  in an enviornment env returning a result of type value. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a value and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type expr -> Env.env -> Env.value for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as eval_e below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)

let rec eval_s (exp : expr) (env : Env.env) : Env.value =
  let val_to_exp (v : Env.value) : expr =
    let Env.Val e = v in
    e in
    match exp with
    | Var _ -> raise (EvalError "free var")
    | Num _ -> Env.Val exp
    | Bool _ -> Env.Val exp
    | Unop (u, x) -> let x' = val_to_exp (eval_s x env) in
                     (match u with
                      | Negate -> (match x' with
                                   | Num n -> Env.Val (Num (~- n))
                                   | Bool b ->
                                       raise (EvalError "negate bool")))
    | Binop (b, x, y) -> let x' = val_to_exp (eval_s x env) in
                         let y' = val_to_exp (eval_s y env) in
                         let int_builder (op : int -> int -> int) =
                           (match x', y' with
                           | Num n, Num m -> Env.Val (Num ((op) n m))
                           | Bool _, Bool _
                           | Num _, Bool _ 
                           | Bool _, Num _ ->
                               raise (EvalError "binop int/bool")) in
                         (match b with
                         | Plus -> int_builder (+)
                         | Minus -> int_builder (-)
                         | Times -> int_builder ( * )
                         | Equals ->
                             (match x', y' with
                              | Num n, Num m -> Env.Val (Bool ((=) n m))
                              | Bool a, Bool b -> Env.Val (Bool ((=) a b))
                              | Num _, Bool _ 
                              | Bool _, Num _ ->
                                  raise (EvalError "compare int/bool"))
                         | LessThan ->
                             (match x', y' with
                              | Num n, Num m -> Env.Val (Bool ((<) n m))
                              | Bool a, Bool b -> Env.Val (Bool ((<) a b))
                              | Num _, Bool _ 
                              | Bool _, Num _ ->
                                  raise (EvalError "compare int/bool")))
    | Conditional (x, y, z) -> let x' = val_to_exp (eval_s x env) in
                               let y' = val_to_exp (eval_s y env) in
                               let z' = val_to_exp (eval_s z env) in
                               (match x' with
                               | Bool bl -> if bl then Env.Val y'
                                            else Env.Val z'
                               | _ ->
                                  raise (EvalError "need bool in conditional"))
    | Fun _ -> Env.Val exp
    | Let (v, x, y) -> let x' = val_to_exp (eval_s x env) in
                       Env.Val (val_to_exp (eval_s (subst v (x') y) env))
    | Letrec (v, x, y) -> let x' = val_to_exp (eval_s x env) in
                          let f1 = subst v x' y in
                          let f2 = subst v (Letrec (v, x', Var "v")) f1 in
                          Env.Val (val_to_exp (eval_s f2 env))
    | Raise -> Env.Val exp
    | Unassigned -> Env.Val exp
    | App (x, y) ->
        match val_to_exp (eval_s x env) with
        | Fun (v, a) -> let y' = val_to_exp (eval_s y env) in
                        Env.Val (val_to_exp (eval_s (subst v (y') a) env))
        | _ -> raise (EvalError "app needs function") ;;


(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)

let rec eval_d (exp : expr) (env : Env.env) : Env.value =
  let val_to_exp (v : Env.value) : expr =
    let Env.Val e = v in
    e in
  match exp with
  | Var v -> Env.lookup env v
  | Num _ -> Env.Val exp
  | Bool _ -> Env.Val exp
  | Unop (u, x) -> let x' = val_to_exp (eval_d x env) in
                   (match u with
                    | Negate -> (match x' with
                                 | Num n -> Env.Val (Num (~- n))
                                 | Bool b -> raise (EvalError "negate bool")))
  | Binop (b, x, y) -> let x' = val_to_exp (eval_d x env) in
                       let y' = val_to_exp (eval_d y env) in
                       let int_builder (op : int -> int -> int) =
                         (match x', y' with
                          | Num n, Num m -> Env.Val (Num ((op) n m))
                          | Bool _, Bool _
                          | Num _, Bool _ 
                          | Bool _, Num _ ->
                              raise (EvalError "binop int/bool")) in
                         (match b with
                          | Plus -> int_builder (+)
                          | Minus -> int_builder (-)
                          | Times -> int_builder ( * )
                          | Equals ->
                              (match x', y' with
                               | Num n, Num m -> Env.Val (Bool ((=) n m))
                               | Bool a, Bool b -> Env.Val (Bool ((=) a b))
                               | Num _, Bool _ 
                               | Bool _, Num _ ->
                                   raise (EvalError "compare int/bool"))
                         | LessThan ->
                             (match x', y' with
                              | Num n, Num m -> Env.Val (Bool ((<) n m))
                              | Bool a, Bool b -> Env.Val (Bool ((<) a b))
                              | Num _, Bool _ 
                              | Bool _, Num _ ->
                                  raise (EvalError "compare int/bool")))
  | Conditional (x, y, z) -> let x' = val_to_exp (eval_d x env) in
                             let y' = val_to_exp (eval_d y env) in
                             let z' = val_to_exp (eval_d z env) in
                             (match x' with
                              | Bool bl -> if bl then Env.Val y'
                                           else Env.Val z'
                              | _ ->
                                  raise (EvalError "need bool in conditional"))
  | Fun _ -> Env.Val exp
  | Let (v, x, y) -> let x' = val_to_exp (eval_d x env) in
                     Env.Val (val_to_exp (eval_d y
                             (Env.extend env v (ref (eval_d x env)))))
  | Letrec (v, x, y) -> failwith "not yet implemented"
  | Raise -> Env.Val exp
  | Unassigned -> Env.Val exp
  | App (x, y) -> failwith "not yet implemented" ;;

(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
  
let eval_l (exp : expr) (env : Env.env) : Env.value =
  match exp with
  | Var v -> Env.lookup env v
  | Num _ -> Env.Val exp
  | Bool _ -> Env.Val exp
  | Unop (u, x) -> failwith "not yet implemented"
  | Binop (b, x, y) -> failwith "not yet implemented"
  | Conditional (x, y, z) -> failwith "not yet implemented"
  | Fun _ -> Env.Val exp
  | Let (v, x, y) -> failwith "not yet implemented"
  | Letrec (v, x, y) -> failwith "not yet implemented"
  | Raise -> Env.Val exp
  | Unassigned -> Env.Val exp
  | App (x, y) -> failwith "not yet implemented" ;;

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within eval_s, eval_d, or eval_l. *)

let eval_e _ =
  failwith "eval_e not implemented" ;;
  
(* Connecting the evaluators to the external world. The REPL in
   miniml.ml uses a call to the single function evaluate defined
   here. Initially, evaluate is the trivial evaluator eval_t. But you
   can define it to use any of the other evaluators as you proceed to
   implement them. (We will directly unit test the four evaluators
   above, not the evaluate function, so it doesn't matter how it's set
   when you submit your solution.) *)
   
let evaluate = eval_s ;;
