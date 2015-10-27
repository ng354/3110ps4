(* Tests infer's functionality *)
open Ast
open TypedAst
open Infer
open Assertions

TEST_UNIT = infer [] Unit === AUnit TUnit
TEST_UNIT = infer [] (Int 7) === AInt (TInt, 7)
TEST_UNIT = infer [] (Bool false) === ABool (TBool, false)
TEST_UNIT = infer [] (String "hello") === AString (TString, "hello")

let () = Pa_ounit_lib.Runtime.summarize()