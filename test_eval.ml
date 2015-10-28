open Ast
open Eval
open Assertions

TEST_UNIT = eval [] (Int 42) === VInt 42

(* Tests Unit, Bool, String *)
TEST_UNIT = eval [] (Unit) === VUnit
TEST_UNIT = eval [] (Bool true) === VBool true
TEST_UNIT = eval [] (Bool false) === VBool false
TEST_UNIT = eval [] (String "hello world") === VString "hello world"

(* Tests BinOp Plus *)
TEST_UNIT = eval [] (BinOp(Plus, Int 2, Int 3)) === VInt 5
TEST_UNIT = eval [] (BinOp(Plus, Int 2,
                      BinOp(Plus, Int 3, Int 4))) === VInt 9

TEST_UNIT = eval [] (BinOp(Plus,
                      BinOp(Plus, Int 3, Int 4), Int 2)) === VInt 9

TEST_UNIT = eval [] (BinOp(Plus,
                      BinOp(Plus, Int 3, Int 4),
                      BinOp(Plus, Int 3, Int 4))) === VInt 14

TEST_UNIT = eval [] (BinOp (Plus, Int 4, Bool true)) ===
                        VError "Not a valid value"

TEST_UNIT = eval [] (BinOp (Plus, String "hey", Bool true)) ===
                        VError "Not a valid value"

TEST_UNIT = eval [] (BinOp (Plus, Bool false, Bool true)) ===
                        VError "Not a valid value"

TEST_UNIT = eval [] (BinOp (Plus, Int 4, Unit)) ===
                        VError "Not a valid value"

(* Test BinOp Minus *)
TEST_UNIT = eval [] (BinOp(Minus, Int 10, Int 2)) === VInt 8
TEST_UNIT = eval [] (BinOp(Minus, Int 10,
                      BinOp(Minus, Int 5, Int 2))) === VInt 7

TEST_UNIT = eval [] (BinOp(Minus,
                      BinOp(Minus,Int 10,Int 1), Int 3)) === VInt 6

TEST_UNIT = eval [] (BinOp(Minus,
                      BinOp(Minus,Int 10,Int 1),
                      BinOp(Minus, Int 5, Int 4))) === VInt 8

TEST_UNIT = eval [] (BinOp(Minus, String "hey", Int 4)) ===
                      VError "Not a valid value"

(* Test combination of Plus and Minus *)
TEST_UNIT = eval [] (BinOp
                      (Minus,
                        BinOp (Plus, Int 3, Int 5),
                        BinOp (Plus, Int 4, Int 1))) === VInt 3

TEST_UNIT = eval [] (BinOp (Minus,
                      BinOp (Plus, Int 3, Int 5), Int 1)) === VInt 7

TEST_UNIT = eval [] (BinOp (Minus,
                      BinOp (Plus, Bool false, Int 5), Int 1)) ===
                        VError "Not a valid value"

(* Test BinOp Times *)
TEST_UNIT = eval [] (BinOp(Times, Int 10, Int 2)) === VInt 20
TEST_UNIT = eval [] (BinOp(Times, Int 10,
                      BinOp(Times, Int 5, Int 2))) === VInt 100

TEST_UNIT = eval [] (BinOp(Times,
                      BinOp(Times,Int 10,Int 1), Int 3)) === VInt 30

TEST_UNIT = eval [] (BinOp(Times,
                      BinOp(Times,Int 10,Int 1),
                      BinOp(Times, Int 5, Int 4))) === VInt 200

TEST_UNIT = eval [] (BinOp(Times, String "hey", Int 4)) ===
                      VError "Not a valid value"

(* Test Binop <,>,<=,>=,=,<> *)
TEST_UNIT = eval [] (BinOp (Lt, Int 4, Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (Lt, Int 10, Int 6)) === VBool false
TEST_UNIT = eval [] (BinOp (Lt, Bool true, Bool true)) === VBool false
TEST_UNIT = eval [] (BinOp (Lt, Unit, Unit)) === VBool false
TEST_UNIT = eval [] (BinOp (Lt, String "hey", String "hey")) === VBool false

TEST_UNIT = eval [] (BinOp (Gt, Int 10, Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (Gt, Int 4, Int 6)) === VBool false
TEST_UNIT = eval [] (BinOp (Gt, Bool true, Bool true)) === VBool false
TEST_UNIT = eval [] (BinOp (Gt, Unit, Unit)) === VBool false
TEST_UNIT = eval [] (BinOp (Gt, String "pce", String "yo")) === VBool false

TEST_UNIT = eval [] (BinOp (Eq, Int 4, Int 6)) === VBool false
TEST_UNIT = eval [] (BinOp (Eq, Int 6, Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (Ast.Eq, String "pce", String "yo")) === VBool false

TEST_UNIT = eval [] (BinOp (GtEq,
                      BinOp(Plus, Int 10, Int 2), Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (GtEq, Int 6, Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (GtEq, Bool true, Bool false)) === VBool true

TEST_UNIT = eval [] (BinOp (LtEq, Int 10, Int 6)) === VBool false
TEST_UNIT = eval [] (BinOp (LtEq,
                      BinOp(Minus, Int 6,Int 4), Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (LtEq, String "hi", String "hi")) === VBool true
TEST_UNIT = eval [] (BinOp (LtEq, Bool true, Bool false)) === VBool false

TEST_UNIT = eval [] (BinOp (NotEq, Int 4, Int 6)) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, Int 6, Int 6)) === VBool false
TEST_UNIT = eval [] (BinOp (NotEq, Bool true, Bool false)) === VBool true
TEST_UNIT = eval [] (BinOp (NotEq, Unit, Bool false)) ===
                      VError "Not a valid value"

TEST_UNIT = eval [] (BinOp (GtEq,
                      BinOp (Times, Int 10,
                          BinOp (Plus, Int 3, Int 5)),
                          BinOp (Minus, Int 14,
                            BinOp (Times, Int 7, Int 1))))=== VBool true

(* Test Concat *)
TEST_UNIT = eval [] (BinOp (Concat,
                      String("hi "), String("bye"))) === VString "hi bye"
TEST_UNIT = eval [] ( BinOp (Concat, Bool true, String "goodbye")) ===
                      VError "Not a valid value"
TEST_UNIT = eval [] (BinOp (Concat, Int 4, Int 5)) ===
                      VError "Not a valid value"

(* End of BinOp tests *)

(* Test if-then-else *)
TEST_UNIT = eval [] (If(Bool true, Int 4, Int 6)) === VInt 4
TEST_UNIT = eval [] (If(Bool false, Int 4, Int 6)) === VInt 6
TEST_UNIT = eval [] (If(Bool false, Int 4, String "false")) === VString "false"
TEST_UNIT = eval [] (If(BinOp (Gt, Int 3, Int 2), Int 4, Int 6)) === VInt 4
TEST_UNIT = eval [] (If (BinOp (NotEq,
                          BinOp (Times, Int 4, Int 7), Int 28),
                        String "wrong",
                        String "right")) === VString "right"

(* Test Var *)
let var_env = ("x", ref (VInt 5))::[]
TEST_UNIT = eval var_env (Var "x") === VInt 5
TEST_UNIT = eval [] (Let ("x",
                      BinOp (Plus, Int 5, Int 6),
                      BinOp (Times, Var "x", Int 8))) === VInt 88

(* Test App, assuming Fun is correct (see below) *)
TEST_UNIT = eval [] (App (Fun ("x",
                            BinOp (Plus, Var "x", Int 1)),
                          BinOp (Plus, Int 5, Int 7))) === VInt 13

TEST_UNIT = eval [] (Fun ("x",
                      App (BinOp (LtEq, Var "x", Int 45),
                          BinOp (Times, Int 9, Int 4)))) ===
                    (VClosure ("x",
                      App (BinOp (LtEq, Var "x", Int 45),
                          BinOp (Times, Int 9, Int 4)), []))

TEST_UNIT = eval [] (App
                      (Fun ("x", BinOp (LtEq, Var "x", Int 45)),
                      BinOp (Times, Int 9, Int 4))) === VBool true

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

TEST_UNIT = eval [("x", ref(VInt 5))]  (Fun ("x",
                                          BinOp (Plus, Var "x", Int 1)) ) ===
                                        VClosure("x",
                                          BinOp (Plus, Var "x", Int 1),
                                          [("x", {contents = VInt 5})] )

(* Test Pair *)
TEST_UNIT = eval [] (Pair (Int 3, Int 6)) === VPair(VInt 3, VInt 6)

TEST_UNIT = eval [] (Pair
                      (BinOp (Plus, Int 3, Int 7),
                      BinOp (Plus, Int 3, Int 6))) === VPair (VInt 10, VInt 9)

TEST_UNIT = eval [] (Pair
                      (App (Fun ("x", BinOp (Plus, Var "x", Int 1)),
                            BinOp (Plus, Int 3, Int 7)),
                      BinOp (Plus, Int 3, Int 6))) ===
                    VPair(VInt 11, VInt 9)

TEST_UNIT = eval [] (Pair
                      (App (Fun ("x", BinOp (Plus, Var "x", Int 1)),
                           BinOp (Plus, Int 3, Int 7)),
                      BinOp (LtEq, Int 3, Int 6))
                    ) === VPair(VInt 11, VBool true)

(* Test Variant *)
TEST_UNIT = eval [] (Variant ("VInt", Int 3)) === VVariant("VInt", VInt 3)
TEST_UNIT = eval [] (Variant ("VBool", BinOp (Lt, Int 3, Int 6))) ===
                    VVariant("VBool", VBool true)

(* Test Let *)
TEST_UNIT = eval [] (Let ("x",
                      Int 5,
                      BinOp (Plus, Var "x", Int 4))) === VInt 9
TEST_UNIT = eval [] (Let ("x",
                      Bool true,
                      If (Var "x", Int 3, Int 5))) === VInt 3
TEST_UNIT = eval [] (Let ("x",
                    BinOp (Times, BinOp (Minus, Int 10, Int 5), Int 4),
                    Let ("y", BinOp (Plus, Var "x", Int 50),
                      If (BinOp
                          (Lt, Var "y", Int 100),
                          Bool true,
                          Bool false)))
                    ) === VBool true

(* Tests principle of name irrelevance *)
TEST_UNIT = eval [] (Let ("x", BinOp (Times,
                                      BinOp (Minus, Int 10, Int 5),
                                      Int 4),
                          Let ("x", BinOp (Plus, Var "x", Int 50),
                          If (BinOp (Ast.Eq, Var "x", Int 70),
                            Bool true,
                            Bool false)))) === VBool true


(* Test LetRec on factorial function *)
TEST_UNIT = eval [] (LetRec ("fact", Fun ("n",
    If (BinOp (Ast.Eq, Var "n", Int 1), Int 1,
     BinOp (Times, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))),
   App (Var "fact", Int 1))) === VInt 1

TEST_UNIT = eval [] (LetRec ("fact", Fun ("n",
    If (BinOp (Ast.Eq, Var "n", Int 1), Int 1,
     BinOp (Times, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))),
   App (Var "fact", Int 4))) === VInt 24

(* Can't compare false and 1 in this context *)
TEST_UNIT = eval [] (LetRec ("fact", Fun ("n",
    If (BinOp (Ast.Eq, Var "n", Int 1), Int 1,
     BinOp (Times, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))),
   App (Var "fact", Bool false))) === VError "Not a boolean value"

(* Test Match *)

(* Match an int on var x *)
TEST_UNIT = eval [("x",ref (VInt 1))] (Match (Int 1,
                [(PInt 1, BinOp (Plus, Var "x", Int 4)); (PBool true, Int 0);
                  (PBool false, Int 100)])) === VInt 5

(* Matches var x which is TRUE with booleans that have expressions *)
TEST_UNIT = eval [("s", ref (VString "not valid"))] (
  Let ("x", BinOp (LtEq, Int 10, Int 40), Match (Var "x",
    [(PBool true, BinOp (LtEq, Int 10, Int 40)); (PBool false, Var "s")])))
  === VBool true

(*Matches Variant on a Variant and then Unit on Unit*)
TEST_UNIT = eval []  (
  Let ("c", Variant ("Cons", Unit),
  Match (Variant ("Cons", Unit),
    [(PVariant ("Decons", PUnit), Int 3); (PVariant ("Cons", PUnit), Int 4);
     (PVar "_", Variant ("PBool", Bool false))]))) === VInt 4

(* Tries to match Variant but then matches PVar *)
TEST_UNIT = eval [] (
  Let ("c", Variant ("Another", Unit),
    Match (Variant ("Another", Unit),
    [(PVariant ("Decons", PUnit), Int 3); (PVariant ("Cons", PUnit), Int 4);
     (PVar "_", Bool false)]))
) === VBool false

let () = Pa_ounit_lib.Runtime.summarize()