open Expr ;;

(* Generate expression examples from textbook *)
let e1 : expr = Num 3 ;;
let e2 : expr = App (Num 3, Num 4) ;;
let e3 : expr = Let ("f", Fun ("x", Var "x"), App (App (Var "f", Var "f"), Num 3)) ;;
let e4 : expr = 
  Letrec ("f", Fun ("x", Conditional (Binop (Equals, Var "x", Num 0), Num 1,
  Binop (Times, Var "x", App (Var "f", Binop(Minus, Var "x", Num 1))))),
  App (Var "f", Num 4)) ;;

(* Tests for exp_to_concrete_string *)
let _ =
  assert (exp_to_concrete_string e1 = "3");
  assert (exp_to_concrete_string e2 = "3 4");
  assert (exp_to_concrete_string e3 = "let f = fun x -> x in f f 3");
  assert (exp_to_concrete_string e4 =
	       "let rec f = fun x -> if (x = 0) then 1 else (x * f (x - 1)) in f 4") ;;

(* Tests for exp_to_abstract_string *)
let _ =
  assert (exp_to_abstract_string e1 = "Num(3)");
  assert (exp_to_abstract_string e2 = "App(Num(3), Num(4))");
  assert (exp_to_abstract_string e3 =
           "Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))");
  assert (exp_to_abstract_string e4 =
           "Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1), Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))), App(Var(f), Num(4)))") ;;

(* Construct additional expressions for testing free_vars *)
let e5 : expr =
  App (Fun ("x", Binop (Plus, Var "x", Var "x")),
  Binop (Times, Num 3, Num 4)) ;;
let e6 : expr =
  Let ("double", Fun ("x", Binop (Times, Num 2, Var "x")),
  App (Var "double", App (Var "double", Num 3))) ;;
let e7 : expr =
  Let ("id", Fun ("x", Var "x"),
  Let ("square", Fun ("x", Binop (Times, Var "x", Var "x")),
  Let ("y", Num 3, App (App (Var "id", Var "square"), Var "y")))) ;;
let e8 : expr =
  Let ("f", Fun ("z", Var "y"), App (Fun("y", App (Var "f", Num 3)), Num 1)) ;;
let e9 : expr =
  Fun ("x", Binop (Plus, Var "x", Var "y")) ;;
let e10 : expr =
  Let ("x", Num 3, Binop (Plus, Var "x", Var "y")) ;;
let e11 : expr =
  Var "y" ;;
let e12 : expr =
  Binop (Plus, Var "x", Var "y") ;;
let e13 : expr =
  Let ("f", App (Var "f", Num 3), Binop (Plus, Var "x", Var "y")) ;;
let e14 : expr =
  App (Fun ("y", Binop (Plus, Var "y", Var "y")), Var "y") ;;
let e15 : expr =
  Fun ("x", Let("x", Var "y", Binop (Plus, Var "x", Num 3))) ;;
let e16 : expr =
  Let ("x", Num 3, Var "x") ;;
let e17 : expr =
  Binop (Plus, Var "y", Var "y") ;;

(* Construct sets for testing free_vars *)
let s1 = vars_of_list [] ;;
let s2 = vars_of_list ["y"] ;;
let s3 = vars_of_list ["x"; "y"] ;;
let s4 = vars_of_list ["f"; "x"; "y"] ;;

(* Tests for free_vars *)
let _ =
  assert (same_vars (free_vars e1) s1);
  assert (same_vars (free_vars e2) s1);
  assert (same_vars (free_vars e3) s1);
  assert (same_vars (free_vars e4) s1);
  assert (same_vars (free_vars e5) s1);
  assert (same_vars (free_vars e6) s1);
  assert (same_vars (free_vars e7) s1);
  assert (same_vars (free_vars e8) s2);
  assert (same_vars (free_vars e9) s2);
  assert (same_vars (free_vars e10) s2);
  assert (same_vars (free_vars e11) s2);
  assert (same_vars (free_vars e12) s3);
  assert (same_vars (free_vars e13) s4);
  assert (same_vars (free_vars e14) s2);
  assert (same_vars (free_vars e15) s2);
  assert (same_vars (free_vars e16) s1);
  assert (same_vars (free_vars e17) s2) ;;

(* Tests for subst *)
let _ =
  assert ((subst "x" (Binop (Plus, Var("x"), Var("x"))) e1) = e1);
  assert ((subst "x" (Binop (Plus, Var("x"), Var("x"))) e2) = e2);
  assert ((subst "x" (Binop (Plus, Var("x"), Var("x"))) e3) = e3);
  assert ((subst "x" (Binop (Plus, Var("x"), Var("x"))) e4) = e4);
  assert ((subst "x" (Binop (Plus, Var("x"), Var("x"))) e5) = e5);
  assert ((subst "x" (Binop (Plus, Var("x"), Var("x"))) e6) = e6);
  assert ((subst "x" (Binop (Plus, Var("x"), Var("x"))) e7) = e7);
  assert ((subst "y" (Num(4)) e8) =
           Let("f", Fun("z", Num(4)), App(Fun("y", App(Var("f"), Num(3))), Num(1))));
  assert ((subst "x" (Binop (Plus, Var("x"), Var("x"))) e9) = e9);
  assert ((subst "y" (Binop (Plus, Var("z"), Var("z"))) e9) =
           Fun("x", Binop(Plus, Var("x"), (Binop (Plus, Var("z"), Var("z")))))) ;;