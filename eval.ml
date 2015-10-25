open Ast

(******************************************************************************)
(** types (see .mli) **********************************************************)
(******************************************************************************)

type value =
  | VUnit | VInt of int | VBool of bool | VString of string
  | VClosure of var * expr * environment
  | VVariant of constructor * value
  | VPair of value * value
  | VError of string
and environment = (var * value ref) list

(******************************************************************************)
(** (optional) helper functions ***********************************************)
(******************************************************************************)

(** you may find it helpful to implement these or other helper
 * functions, but they are not required. Feel free to implement them if you
 * need them, change their types or arguments, delete them, whatever.
 *)

 (**
  * try to match a value against a pattern. If the match succeeds, return an
  * environment containing all of the bindings. If it fails, return None.
  *)

let rec find_match (p : pattern) (v : value) : environment option =
  failwith "unimplemented"

(** apply the given operator to the given arguments *)
let rec eval_operator (op : operator) (v1 : value) (v2 : value) : value =
  failwith "unimplemented"

(** Format a value for printing. *)
let rec format_value (f : Format.formatter) (v : value) : unit =
  (* You will probably want to call Format.fprint f f <format string> <args>.
   *
   * Format.fprintf f <format string> has a different type depeding on the format
   * string. For example, Format.fprintf f "%s" has type string -> unit, while
   * Format.fprintf f "%i" has type int -> unit.
   *
   * Format.fprintf f "%a" is also useful. It has type
   *   (Format.formatter -> 'a -> unit) -> 'a -> unit
   * which is useful for recursively formatting values.
   *
   * Format strings can contain multiple flags and also other things to be
   * printed. For example (Format.fprintf f "result: %i %s") has type
   * int -> string -> unit, so you can write
   *
   *  Format.fprintf f "result: %i %s" 3 "blind mice"
   *
   * to output "result: 3 blind mice"
   *
   * See the documentation of the OCaml Printf module for the list of % flags,
   * and see the printer.ml for some (complicated) examples. Printer, format_type is
   * a nice example.
   *)
  failwith "unimplemented"

(** use format_value to print a value to the console *)
let print_value = Printer.make_printer format_value

(** use format_value to convert a value to a string *)
let string_of_value = Printer.make_string_of format_value

(******************************************************************************)
(** eval **********************************************************************)
(******************************************************************************)

let rec bin_operation op e1 e2 =
  match (op, e1, e2) with
  | (Plus, VInt n1, VInt n2) -> VInt (n1+n2)
  | (Minus, VInt n1, VInt n2) -> VInt (n1-n2)
  | (Times, VInt n1, VInt n2) -> VInt (n1*n2)
  | (Gt, VInt n1, VInt n2) -> VBool(n1>n2)
  | (Lt, VInt n1, VInt n2) -> VBool(n1<n2)
  | (Eq, VInt n1, VInt n2) -> VBool(n1=n2)
  | (GtEq, VInt n1, VInt n2) -> VBool(n1>=n2)
  | (LtEq, VInt n1, VInt n2) -> VBool(n1<=n2)
  | (NotEq, VInt n1, VInt n2) -> VBool(n1<>n2)
  | (Concat, VString s1, VString s2) -> VString(s1^s2)
  | _-> VError "Not a valid value"

let rec if_then e1 e2 e3 =
  match e1 with
  | VBool true -> e2
  | VBool false -> e3
  | _ -> VError "Not a valid value"


(* let rec appl env e1 e2 = eval e1 and match on that vclosure (var, expr, env) change env in vlosure case
and call eval on new environment and e1.
e2 used in making new environment
var e1 and another env in VClosure. then call eval again, have a new environment, ref var e2:: envr closure
  match (e1, e2) with
  | *)

let rec eval env e =
  match e with
  | Int n ->  VInt(n)
  | Bool b -> VBool(b)
  | String s ->  VString(s)
  | BinOp (op, e1,e2)  ->
    let expr1 = eval env e1 in
    let expr2 = eval env e2 in
    bin_operation op expr1 expr2
  | If (e1,e2,e3) ->
    let expr1 = eval env e1 in
    let expr2 = eval env e2 in
    let expr3 = eval env e3 in
    if_then expr1 expr2 expr3
  | Var x -> !(List.assoc x env)
  | Let (v,e1,e2) -> failwith "unimplemented"
  | LetRec (v,e1,e2) -> failwith "unimplemented"
  | App(e1,e2) -> failwith "unimplemented" (*match on the function call match with Vclosure*)
    (* let expr1 = eval env e1 in
    let expr2 = eval env e2 in
    appl env expr1 expr2 *)
  | Fun (v,e) -> failwith "unimplemented"
  | Pair (e1,e2) -> failwith "unimplemented"
  | Variant (c, e1) -> failwith "unimplemented"
  | Match (e1, p) -> failwith "unimplemented"
  | _ -> failwith "unimplemented"

