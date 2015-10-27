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
  match p,v with
  | PUnit, VUnit -> Some []
  | PInt n1, VInt n2 ->
    if n1 = n2 then Some [] else None
  | PBool b1, VBool b2 ->
    if b1=b2 then Some [] else None
  | PString s1, VString s2 ->
    if s1=s2 then Some [] else None
  | PVar v1, _ ->
    Some [(v1, ref v)]
  | PVariant(c1,p1), VVariant(c2,e2) ->
    if c1=c2 then
      find_match p1 e2
    else
      None
  | PPair(p1,p2), VPair(e1,e2) ->
    let env1 =
      (match find_match p1 e1 with
      | Some p -> p
      | None -> [])
    in
    let env2 =
      (match find_match p2 e2 with
      | Some p -> p
      | None -> [])
    in
    Some (env1@env2)
  | _ -> None


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
  | (Gt, VString s1, VString s2) -> VBool(s1>s2)
  | (Gt, VBool b1, VBool b2) -> VBool(b1>b2)
  | (Gt, VUnit, VUnit) -> VBool(false)
  | (Lt, VInt n1, VInt n2) -> VBool(n1<n2)
  | (Lt, VString s1, VString s2) -> VBool(s1<s2)
  | (Lt, VBool b1, VBool b2) -> VBool(b1<b2)
  | (Lt, VUnit, VUnit) -> VBool(false)
  | (Eq, VInt n1, VInt n2) -> VBool(n1=n2)
  | (Eq, VString s1, VString s2) -> VBool(s1=s2)
  | (Eq, VBool b1, VBool b2) -> VBool(b1=b2)
  | (Eq, VUnit, VUnit) -> VBool(true)
  | (GtEq, VInt n1, VInt n2) -> VBool(n1>=n2)
  | (GtEq, VString s1, VString s2) -> VBool(s1>=s2)
  | (GtEq, VBool b1, VBool b2) -> VBool(b1>=b2)
  | (GtEq, VUnit, VUnit) -> VBool(true)
  | (LtEq, VInt n1, VInt n2) -> VBool(n1<=n2)
  | (LtEq, VString s1, VString s2) -> VBool(s1<=s2)
  | (LtEq, VBool b1, VBool b2) -> VBool(b1<=b2)
  | (LtEq, VUnit, VUnit) -> VBool(true)
  | (NotEq, VInt n1, VInt n2) -> VBool(n1<>n2)
  | (NotEq, VString s1, VString s2) -> VBool(s1<>s2)
  | (NotEq, VBool b1, VBool b2) -> VBool(b1<>b2)
  | (NotEq, VUnit, VUnit) -> VBool(false)
  | (Concat, VString s1, VString s2) -> VString(s1^s2)
  | _-> VError "Not a valid value"


(* [change_env env v e1] Returns new env without binding x, if x not in list,
* returns dup. Then adds the new binding (x,e2) to the environment for e1
* - [env] the previous environment before change_env was called
* - [v] the string variable name
* - [e1] the expression that will be bound to v *)
let change_env env v e1 =
  try
    let () = (List.assoc v env):= e1 in
    env
  with
    Not_found -> (v,ref e1)::env

(* [app_env env e1 e2] will return new environment for the function e1
* with the new binding from e2 so that we can make the appropriate closure *)
let app_env env e1 e2 =
  match e1 with
  | Fun(x,e) ->
    change_env env x e2
  | _ -> env

(* [match_patterns v plist] finds the matching pattern of
* - value [v] with
* - pattern list [plist]
* - returns an option of the  pattern's evaluation expression
*   and its environment *)
let rec match_patterns (v:value) (plist:(pattern*expr) list)
                        : ((environment*expr) option) =
  match plist with
  | [] -> None
  | (p, e)::t ->
    (match find_match p v with
      | Some env ->
        Some (env,e)
      | None -> match_patterns v t)

let rec eval env e =
  match e with
  | Unit -> VUnit
  | Int n ->  VInt(n)
  | Bool b -> VBool(b)
  | String s ->  VString(s)
  | BinOp (op, e1,e2)  ->
    let expr1 = eval env e1 in
    let expr2 = eval env e2 in
    bin_operation op expr1 expr2
  | If (e1,e2,e3) ->
    let expr1 = eval env e1 in
    (match expr1 with
      | VBool true -> eval env e2
      | VBool false -> eval env e3
      | _ -> VError "Not a boolean value")
  | Var x ->
    (try
      !(List.assoc x env)
    with
      Not_found -> VError (x^"variable not found"))
  | Let (v,e1,e2) ->
    let expr1 = eval env e1 in
    let new_env = change_env env v expr1 in
    eval new_env e2
  | LetRec (v,e1,e2) ->
    let v_ref = ref (VError "dummy") in
    let new_env = (v,v_ref)::env in
    let evaluated = eval new_env e1 in
    v_ref:= evaluated;
    eval new_env e2
  | App(e1,e2) ->
    let expr2 = eval env e2 in
    (match (eval env e1) with
    | VClosure(var, expr, env) -> eval (change_env env var expr2) expr
    | _ -> VError "Not a function, cannot be applied")
  | Fun(v,e) ->  VClosure(v, e, env)
  | Pair (e1,e2) ->
    let expr1 = eval env e1 in
    let expr2 = eval env e2 in
    VPair(expr1, expr2)
  | Variant (c, e1) ->
    let expr1 = eval env e1 in
    VVariant(c, expr1)
  | Match (e1, p) ->
    let expr1 = eval env e1 in
    (match match_patterns expr1 p with
        | Some (sub_env,expr) ->  eval (sub_env@env) expr
        | None -> VError "No pattern matched")
