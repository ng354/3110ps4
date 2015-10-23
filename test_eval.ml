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



let () = Pa_ounit_lib.Runtime.summarize()