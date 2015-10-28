(* Tests infer's functionality *)
open Ast
open TypedAst
open Infer
open Assertions

TEST_UNIT = infer [] Unit === AUnit TUnit
TEST_UNIT = infer [] (Int 7) === AInt (TInt, 7)
TEST_UNIT = infer [] (Bool false) === ABool (TBool, false)
TEST_UNIT = infer [] (String "hello") === AString (TString, "hello")

(* Test App which also checks Fun and Var *)
TEST_UNIT = infer [] (App (Fun ("x", BinOp (Plus, Var "x", Int 1)), Int 1)) ===
                      AApp (TInt, AFun (TArrow (TInt, TInt), ("x", TInt),
                        ABinOp (TInt, Plus, AVar (TInt, "x"), AInt (TInt, 1))),
                       AInt (TInt, 1))


(* Test function , which also checks Var*)
TEST_UNIT = infer [] (Fun ("x", BinOp (Plus, Var "x", Int 1))) ===
  AFun (TArrow (TInt, TInt), ("x", TInt),
    ABinOp (TInt, Plus, AVar (TInt, "x"), AInt (TInt, 1)))

TEST_UNIT = infer [] (Fun ("x", Var "x")) === AFun (TArrow (TAlpha "a", TAlpha "a"), ("x", TAlpha "a"), AVar (TAlpha "a", "x"))

(* Test Let *)
TEST_UNIT = infer [] (Let ("x", Int 5, BinOp (Plus, Var "x", Int 7))) ===
                      ALet (TInt, ("x", TInt), AInt (TInt, 5),
                        ABinOp (TInt, Plus, AVar (TInt, "x"), AInt (TInt, 7)))

(* Test LetRec *)
TEST_UNIT = infer [] (LetRec ("fact",                                                                                      Fun ("n",
    If (BinOp (Ast.Eq, Var "n", Int 1), Int 1,
     BinOp (Times, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))),
   App (Var "fact", Int 4))) ===
     (ALetRec (TInt, ("fact", TArrow (TInt, TInt)),                                                        AFun (TArrow (TInt, TInt), ("n", TInt),
    AIf (TInt, ABinOp (TBool, Ast.Eq, AVar (TAlpha "t13", "n"), AInt (TAlpha "t12", 1)),
     AInt (TInt, 1),
     ABinOp (TInt, Times, AVar (TInt, "n"),
      AApp (TInt, AVar (TArrow (TInt, TInt), "fact"),
       ABinOp (TInt, Minus, AVar (TInt, "n"), AInt (TInt, 1)))))),
   AApp (TInt, AVar (TArrow (TInt, TInt), "fact"), AInt (TInt, 4))))


(* Test pairs *)
TEST_UNIT = infer [] (Pair (Int 4, Int 5)) === (APair (TStar (TInt, TInt), AInt (TInt, 4), AInt (TInt, 5)))
TEST_UNIT = infer [] (Pair (Int 4, Bool false)) === (APair (TStar (TInt, TBool), AInt (TInt, 4), ABool (TBool, false)))

(* Test Binops *)
(* Test Plus *)
TEST_UNIT = infer [] (BinOp (Plus, Int 5, Int 100)) === (ABinOp (TInt, Plus, AInt (TInt, 5), AInt (TInt, 100)) )
(* BinOp (Plus, Bool false, Bool true)    *)
(* BinOp (Plus, Unit, Int 4) *)

(* Test Times *)
TEST_UNIT = infer [] (BinOp (Times, Int 4, Int 16)) === (ABinOp (TInt, Times, AInt (TInt, 4), AInt (TInt, 16)))
(* BinOp (Times, Bool true, Unit)  *)

(* Test Minus *)
TEST_UNIT = infer [] (BinOp (Minus, Int 5, Int 4)) === ABinOp (TInt, Minus, AInt (TInt, 5), AInt (TInt, 4))
(* BinOp (Minus, Int 5, Unit) *)

(* Test plus,minus, times *)
TEST_UNIT = infer [] (BinOp (Times, BinOp (Plus, Int 4, Int 5), BinOp (Minus, Int 8, Int 3))) ===
ABinOp (TInt, Times, ABinOp (TInt, Plus, AInt (TInt, 4), AInt (TInt, 5)), ABinOp (TInt, Minus, AInt (TInt, 8), AInt (TInt, 3)))

(* Test concatenation *)
TEST_UNIT = infer [] (BinOp (Concat, String "hello", String " goodbye")) === (ABinOp (TString, Concat, AString (TString, "hello"),
                                AString (TString, " goodbye")) )
 (* BinOp (Concat, String "hello", Int 3)  *)

 (* Test <, >, <=, >=  *)
 TEST_UNIT = infer [] (BinOp (Lt, Int 4, Int 6)) === (ABinOp (TBool, Lt, AInt (TInt, 4), AInt (TInt, 6)))
(* BinOp (Lt, Int 4, Bool false) *)
TEST_UNIT = infer [] (BinOp (Gt, Bool true, Bool false)) === (ABinOp (TBool, Gt, ABool (TBool, true), ABool (TBool, false)))
TEST_UNIT = infer [] (BinOp (GtEq, Int 5, Int 5)) === (ABinOp (TBool, GtEq, AInt (TInt, 5), AInt (TInt, 5)))
TEST_UNIT = infer [] (BinOp (LtEq, Unit, Unit) ) ===  (ABinOp (TBool, LtEq, AUnit TUnit, AUnit TUnit))
TEST_UNIT = infer [] (BinOp (NotEq, BinOp (Plus, Int 4, Int 9), BinOp (Plus, Int 12, Int 1))) === (ABinOp (TBool, NotEq, ABinOp (TInt, Plus, AInt (TInt, 4), AInt (TInt, 9)),
                                       ABinOp (TInt, Plus, AInt (TInt, 12), AInt (TInt, 1))) )
TEST_UNIT = infer [] (BinOp (Ast.Eq, BinOp (Plus, Int 4, Int 9), BinOp (Plus, Int 12, Int 1)) ) === (ABinOp (TBool, Ast.Eq, ABinOp (TInt, Plus, AInt (TInt, 4), AInt (TInt, 9)),
                                              ABinOp (TInt, Plus, AInt (TInt, 12), AInt (TInt, 1))))


(* Test if-then-else *)
TEST_UNIT = infer [] (If (Bool true, Int 5, Int 6)) === (AIf (TAlpha "a", ABool (TBool, true), AInt (TInt, 5), AInt (TInt, 6)))
(* FIGURE OUT HOW TO TEST AN EXCEPTION?????? *)
(* TEST_UNIT = infer [] (If (Bool true, Int 5, Bool false)) ===  *)


let () = Pa_ounit_lib.Runtime.summarize()