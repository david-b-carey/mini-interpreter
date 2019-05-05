open Expr ;;

(* Generate expression examples from textbook *)

let e1 : expr = Num(3) ;;
let e2 : expr = App(Num(3), Num(4)) ;;
let e3 : expr = Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3))) ;;
let e4 : expr = 
  Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
  Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
  App(Var("f"), Num(4))) ;;

(* Tests for exp_to_concrete_string *)
let _ =
  assert (exp_to_concrete_string e1 = "3");
  assert (exp_to_concrete_string e2 = "(3) (4)");
  assert (exp_to_concrete_string e3 = "let f = fun x -> x in ((f) (f)) (3)");
  assert (exp_to_concrete_string e4 =
	       "let rec f = fun x -> if x = 0 then 1 else (x) * ((f) (x - (1))) in (f) (4)") ;;

(* Tests for exp_to_abstract_string *)
let _ =
  assert (exp_to_abstract_string e1 = "Num(3)");
  assert (exp_to_abstract_string e2 = "App(Num(3), Num(4))");
  assert (exp_to_abstract_string e3 =
           "Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))");
  assert (exp_to_abstract_string e4 =
           "Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1), Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))), App(Var(f), Num(4)))") ;;