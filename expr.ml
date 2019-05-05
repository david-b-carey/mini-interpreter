(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal ;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let free_vars (exp : expr) : varidset =
  let rec builder (exp : expr) (vs : varidset) : varidset =
    match exp with
    | Var(x) -> SS.add x vs
    | Num _ -> vs
    | Bool _ -> vs
    | Binop(_, x, y) -> SS.union (builder x vs) (builder y vs)
    | Fun(v, x) -> SS.remove v (builder x vs)
    | Let(v, x, y) -> SS.union (SS.remove v (builder y vs)) (builder x vs)
    | App(f, x) -> SS.union (builder f vs) (builder x vs)
  in
  builder exp SS.empty ;;
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;

(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var(x) -> "" ^ x
  | Num(x) -> string_of_int x
  | Bool(x) -> string_of_bool x
  | Unop(u, x) -> (match u with
                  | Negate -> "~- (" ^
                              exp_to_concrete_string x) ^ ")"
  | Binop(b, x, y) -> (match b with
                      | Plus -> exp_to_concrete_string x ^ " + " ^
                                exp_to_concrete_string y
                      | Minus -> exp_to_concrete_string x ^ " - (" ^
                                 exp_to_concrete_string y ^ ")"
                      | Times -> "(" ^ exp_to_concrete_string x ^ ") * (" ^
                                 exp_to_concrete_string y ^ ")"
                      | Equals -> exp_to_concrete_string x ^ " = " ^
                                  exp_to_concrete_string y
                      | LessThan -> exp_to_concrete_string x ^ " < " ^
                                    exp_to_concrete_string y)
  | Conditional(x, y, z) -> "if " ^ exp_to_concrete_string x ^ " then " ^
                            exp_to_concrete_string y ^ " else " ^
                            exp_to_concrete_string z
  | Fun(v, x) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string x
  | Let(v, x, y) -> "let " ^ v ^ " = " ^ exp_to_concrete_string x ^ " in " ^
                    exp_to_concrete_string y
  | Letrec(v, x, y) -> "let rec " ^ v ^ " = " ^ exp_to_concrete_string x ^
                       " in " ^ exp_to_concrete_string y
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App(f, x) -> "(" ^ exp_to_concrete_string f ^ ") (" ^
                 exp_to_concrete_string x ^ ")" ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var(x) -> "Var(" ^ x ^ ")"
  | Num(x) -> "Num(" ^ string_of_int x ^ ")"
  | Bool(x) -> "Bool(" ^ string_of_bool x ^ ")"
  | Unop(u, x) -> (match u with
                  | Negate -> "Unop(Negate, " ^
                              exp_to_abstract_string x ^ ")")
  | Binop(b, x, y) -> (match b with
                      | Plus -> "Binop(Plus, " ^
                                exp_to_abstract_string x ^ ", " ^
                                exp_to_abstract_string y ^ ")"
                      | Minus -> "Binop(Minus, " ^
                                 exp_to_abstract_string x ^ ", " ^
                                 exp_to_abstract_string y ^ ")"
                      | Times -> "Binop(Times, " ^
                                 exp_to_abstract_string x ^ ", " ^
                                 exp_to_abstract_string y ^ ")"
                      | Equals -> "Binop(Equals, " ^
                                  exp_to_abstract_string x ^ ", " ^
                                  exp_to_abstract_string y ^ ")"
                      | LessThan -> "Binop(LessThan, " ^
                                    exp_to_abstract_string x ^ ", " ^
                                    exp_to_abstract_string y ^ ")")
  | Conditional(x, y, z) -> "Conditional(" ^ exp_to_abstract_string x ^ ", " ^
                            exp_to_abstract_string y ^ ", " ^
                            exp_to_abstract_string z ^ ")"
  | Fun(v, x) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string x ^ ")"
  | Let(v, x, y) -> "Let(" ^ v ^ ", " ^ exp_to_abstract_string x ^ ", " ^
                    exp_to_abstract_string y ^ ")"
  | Letrec(v, x, y) -> "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string x ^ ", " ^
                       exp_to_abstract_string y ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App(f, x) -> "App(" ^ exp_to_abstract_string f ^ ", " ^
                 exp_to_abstract_string x ^ ")" ;;
