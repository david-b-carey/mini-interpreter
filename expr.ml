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
  let vs : varidset = SS.empty in
  let rec builder (exp : expr) (vs : varidset) : varidset =
    match exp with
    | Var v -> SS.singleton v
    | Num _ -> vs
    | Bool _ -> vs
    | Unop (_, x) -> builder x vs
    | Binop (_, x, y) -> SS.union (builder x vs) (builder y vs)
    | Conditional (x, y, z) ->
        SS.union (builder x vs) (SS.union (builder y vs) (builder z vs))
    | Fun (v, x) -> SS.remove v (builder x vs)
    | Let (v, x, y) ->
        SS.union (SS.remove v (builder y vs)) (builder x vs)
    | Letrec (v, x, y) ->
        SS.union (SS.remove v (builder y vs)) (SS.remove v (builder x vs))
    | Raise -> vs
    | Unassigned -> vs
    | App (x, y) -> SS.union (builder x vs) (builder y vs) in
  builder exp vs ;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let ctr = ref 0 ;;
let new_varname () : varid =
  let name = "x" ^ string_of_int !ctr in
  ctr := !ctr + 1;
  name ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  match exp with
  | Var v -> if v = var_name then repl else Var v
  | Num x -> Num x
  | Bool x -> Bool x
  | Unop (u, x) -> Unop (u, (subst var_name repl x))
  | Binop (b, x, y) -> Binop (b, (subst var_name repl x), (subst var_name repl y))
  | Conditional (x, y, z) ->
      Conditional ((subst var_name repl x), (subst var_name repl y), (subst var_name repl z))
  | Fun (v, x) -> if v = var_name then Fun (v, x)
                  else if (v <> var_name) &&
                          (not (SS.mem v (free_vars repl)))
                    then Fun (v, subst var_name repl x)
                  else
                    let newvar = new_varname () in
                    Fun (newvar, (subst var_name repl (subst v (Var newvar) x)))
  | Let (v, x, y) -> if v = var_name
                       then Let (v, (subst var_name repl x), y)
                     else if (v <> var_name) &&
                             (not (SS.mem v (free_vars repl)))
                       then Let (v, (subst var_name repl x), (subst var_name repl y))
                     else
                       let newvar = new_varname () in
                       Let (newvar, (subst var_name repl x),
                       (subst var_name repl (subst v (Var newvar) y)))
  | Letrec (v, x, y) -> if v = var_name
                          then Letrec (v, x, y)
                        else
                          Letrec(v, (subst var_name repl x), (subst var_name repl y))
  | Raise -> Raise
  | Unassigned -> Unassigned
  | App (x, y) -> App ((subst var_name repl x), (subst var_name repl y)) ;;

(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  (* Note: Technically, since varids are of type string, the user could
     input them as strings broken up by spaces, such as "v x". This could
     make my concrete string representations appear a bit awkward, but
     I have gathered that we can assume each varid to be represented in
     a contiguous, character-by-character string. *)
  match exp with
  | Var v -> "" ^ v
  | Num x -> string_of_int x
  | Bool x -> string_of_bool x
  | Unop (u, x) -> (match u with
                   | Negate -> "~- (" ^
                               exp_to_concrete_string x ^ ")")
  | Binop (b, x, y) -> (match b with
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
  | Conditional (x, y, z) -> "if " ^ exp_to_concrete_string x ^ " then " ^
                             exp_to_concrete_string y ^ " else " ^
                             exp_to_concrete_string z
  | Fun (v, x) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string x
  | Let (v, x, y) -> "let " ^ v ^ " = " ^ exp_to_concrete_string x ^ " in " ^
                     exp_to_concrete_string y
  | Letrec (v, x, y) -> "let rec " ^ v ^ " = " ^ exp_to_concrete_string x ^
                        " in " ^ exp_to_concrete_string y
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (x, y) -> "(" ^ exp_to_concrete_string x ^ ") (" ^
                  exp_to_concrete_string y ^ ")" ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var(" ^ v ^ ")"
  | Num x -> "Num(" ^ string_of_int x ^ ")"
  | Bool x -> "Bool(" ^ string_of_bool x ^ ")"
  | Unop (u, x) -> (match u with
                   | Negate -> "Unop(Negate, " ^
                               exp_to_abstract_string x ^ ")")
  | Binop (b, x, y) -> (match b with
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
  | Conditional (x, y, z) -> "Conditional(" ^ exp_to_abstract_string x ^ ", " ^
                             exp_to_abstract_string y ^ ", " ^
                             exp_to_abstract_string z ^ ")"
  | Fun (v, x) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string x ^ ")"
  | Let (v, x, y) -> "Let(" ^ v ^ ", " ^ exp_to_abstract_string x ^ ", " ^
                     exp_to_abstract_string y ^ ")"
  | Letrec (v, x, y) -> "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string x ^ ", " ^
                        exp_to_abstract_string y ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (x, y) -> "App(" ^ exp_to_abstract_string x ^ ", " ^
                  exp_to_abstract_string y ^ ")" ;;