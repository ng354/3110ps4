open Ast
open Eval
open Assertions

TEST_UNIT = eval [] (Int 42) === VInt 42

(* TODO: write more unit tests for [eval] *)

(* Tests Bool, String *)
TEST_UNIT = eval [] (Bool true) === VBool true
TEST_UNIT = eval [] (Bool false) === VBool false
TEST_UNIT = eval [] (String "hello world") === VString "hello world"

(* Tests BinOp Plus *)
TEST_UNIT = eval [] (BinOp(Plus, Int 2, Int 3)) === VInt 5
TEST_UNIT = eval [] (BinOp(Plus, Int 2, BinOp(Plus, Int 3, Int 4))) === VInt 9
TEST_UNIT = eval [] (BinOp(Plus, BinOp(Plus, Int 3, Int 4), Int 2)) === VInt 9
TEST_UNIT = eval [] (BinOp(Plus, BinOp(Plus, Int 3, Int 4), BinOp(Plus, Int 3, Int 4))) === VInt 14
TEST_UNIT = eval [] (BinOp (Plus, Int 4, Bool true)) === VError "Not a valid value"

(* Test BinOp Minus *)
TEST_UNIT = eval [] (BinOp(Minus, Int 10, Int 2)) === VInt 8
TEST_UNIT = eval [] (BinOp(Minus, Int 10, BinOp(Minus, Int 5, Int 2))) === VInt 7
TEST_UNIT = eval [] (BinOp(Minus, BinOp(Minus,Int 10,Int 1), Int 3)) === VInt 6
TEST_UNIT = eval [] (BinOp(Minus, BinOp(Minus,Int 10,Int 1), BinOp(Minus, Int 5, Int 4))) === VInt 8
TEST_UNIT = eval [] (BinOp(Minus, String "hey", Int 4)) === VError "Not a valid value"

(* Test combination of Plus and Minus *)
TEST_UNIT = eval [] (BinOp (Minus, BinOp (Plus, Int 3, Int 5), BinOp (Plus, Int 4, Int 1))) === VInt 3
TEST_UNIT = eval [] (BinOp (Minus, BinOp (Plus, Int 3, Int 5), Int 1)) === VInt 7

(* Test BinOp Times *)
TEST_UNIT = eval [] (BinOp(Times, Int 10, Int 2)) === VInt 20
TEST_UNIT = eval [] (BinOp(Times, Int 10, BinOp(Times, Int 5, Int 2))) === VInt 100
TEST_UNIT = eval [] (BinOp(Times, BinOp(Times,Int 10,Int 1), Int 3)) === VInt 30
TEST_UNIT = eval [] (BinOp(Times, BinOp(Times,Int 10,Int 1), BinOp(Times, Int 5, Int 4))) === VInt 200
TEST_UNIT = eval [] (BinOp(Times, String "hey", Int 4)) === VError "Not a valid value"

(* Test Binop <,>,<=,>=,=,<> *)
TEST_UNIT = eval [] (BinOp (Lt, Int 4, Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (Lt, Int 10, Int 6)) === VBool false

TEST_UNIT = eval [] (BinOp (Gt, Int 10, Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (Gt, Int 4, Int 6)) === VBool false

TEST_UNIT = eval [] (BinOp (Eq, Int 4, Int 6)) === VBool false
TEST_UNIT = eval [] (BinOp (Eq, Int 6, Int 6)) === VBool true

TEST_UNIT = eval [] (BinOp (GtEq, BinOp(Plus, Int 10, Int 2), Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (GtEq, Int 6, Int 6)) === VBool true

TEST_UNIT = eval [] (BinOp (LtEq, Int 10, Int 6)) === VBool false
TEST_UNIT = eval [] (BinOp (LtEq, BinOp(Minus, Int 6,Int 4), Int 6)) === VBool true

TEST_UNIT = eval [] (BinOp (NotEq, Int 4, Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, Int 6, Int 6)) === VBool false

TEST_UNIT = eval [] (BinOp (GtEq, BinOp (Times, Int 10, BinOp (Plus, Int 3, Int 5)),
                          BinOp (Minus, Int 14, BinOp (Times, Int 7, Int 1))))=== VBool true

(* Test Concat *)
TEST_UNIT = eval [] (BinOp (Concat, String("hello "), String("goodbye"))) === VString "hello goodbye"
TEST_UNIT = eval [] ( BinOp (Concat, Bool true, String "goodbye")) === VError "Not a valid value"

(* End of BinOp tests *)

(* Test if-then-else *)
TEST_UNIT = eval [] (If(Bool true, Int 4, Int 6)) === VInt 4
TEST_UNIT = eval [] (If(Bool false, Int 4, Int 6)) === VInt 6
TEST_UNIT = eval [] (If(Bool false, Int 4, String "false!!")) === VString "false!!"
TEST_UNIT = eval [] (If(BinOp (Gt, Int 3, Int 2), Int 4, Int 6)) === VInt 4

(* Test Var *)
let var_env = ("x", ref (VInt 5))::[]
TEST_UNIT = eval var_env (Var "x") === VInt 5

(* Test App, assuming Fun is correct (see below) *)
TEST_UNIT = eval [] (App (Fun ("x", BinOp (Plus, Var "x", Int 1)), BinOp (Plus, Int 5, Int 7))) === VInt 13

TEST_UNIT = eval [] (
 App (Fun ("x", BinOp (Plus, Var "x", Int 1)),
  App (Fun ("y", BinOp (Plus, Var "y", Int 10)), Int 5))) === VInt 16

TEST_UNIT = eval [] (
App
 (App (Fun ("x", BinOp (Plus, Var "x", Int 1)),
     Fun ("y", BinOp (Plus, Var "y", Int 10))),
   Int 5)) === VError "Not a function, cannot be applied"


(* Test Fun *)
TEST_UNIT = eval [] (Fun ("x", BinOp (Plus, Var "x", Int 1)) ) ===
  VClosure("x", BinOp (Plus, Var "x", Int 1), [])

TEST_UNIT = eval [("x", ref(VInt 5))]  (Fun ("x", BinOp (Plus, Var "x", Int 1)) ) ===
  VClosure("x",  BinOp (Plus, Var "x", Int 1), [("x", {contents = VInt 5})] )

(* Test Pair *)
TEST_UNIT = eval [] (Pair (Int 3, Int 6)) === VPair(VInt 3, VInt 6)
TEST_UNIT = eval [] (Pair (BinOp (Plus, Int 3, Int 7), BinOp (Plus, Int 3, Int 6)) )
  === VPair (VInt 10, VInt 9)
TEST_UNIT = eval [] (Pair (App (Fun ("x", BinOp (Plus, Var "x", Int 1)), BinOp (Plus, Int 3, Int 7)),
  BinOp (Plus, Int 3, Int 6))  ) === VPair(VInt 11, VInt 9)
TEST_UNIT = eval [] (Pair (App (Fun ("x", BinOp (Plus, Var "x", Int 1)), BinOp (Plus, Int 3, Int 7)),
  BinOp (LtEq, Int 3, Int 6))   ) === VPair(VInt 11, VBool true)

(* Test Variant *)
TEST_UNIT = eval []  (Variant ("VInt", Int 3)) === VVariant("VInt", VInt 3)
TEST_UNIT = eval [] ( Variant ("VBool", BinOp (Lt, Int 3, Int 6)) ) === VVariant("VBool", VBool true)

(* Test Let *)
TEST_UNIT = eval [] (Let ("x", Int 5, BinOp (Plus, Var "x", Int 4))) === VInt 9
TEST_UNIT = eval [] (Let ("x", Bool true, If (Var "x", Int 3, Int 5))) === VInt 3
TEST_UNIT = eval [] (Let ("x", BinOp (Times, BinOp (Minus, Int 10, Int 5), Int 4),                            Let ("y", BinOp (Plus, Var "x", Int 50),
    If (BinOp (Lt, Var "y", Int 100), Bool true, Bool false)))) === VBool true
(* Tests principle of name irrelevance *)
TEST_UNIT = eval [] (Let ("x", BinOp (Times, BinOp (Minus, Int 10, Int 5), Int 4),                            Let ("x", BinOp (Plus, Var "x", Int 50),
    If (BinOp (Ast.Eq, Var "x", Int 70), Bool true, Bool false)))) === VBool true

(* Test Match *)
let map  = Parse.parse_expr "let rec map = fun f -> fun l -> match l with
                                | Nil ()       -> Nil ()
                                | Cons (hd,tl) -> Cons (f hd, map f tl)
                              in map"
let map' = LetRec ("map", Fun ("f", Fun ("l",
                            Match (Var "l", [
                              PVariant ("Nil",  PUnit),
                                Variant ("Nil", Unit);
                              PVariant ("Cons", PPair(PVar "hd", PVar "tl")),
                                Variant ("Cons", Pair (App (Var "f", Var "hd")
                                                      ,App (App (Var "map", Var "f"), Var "tl")
                                                      ));
                            ]))),
                   Var "map")

TEST_UNIT = map === map'


let () = Pa_ounit_lib.Runtime.summarize()