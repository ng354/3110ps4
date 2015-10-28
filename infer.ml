open Ast
open TypedAst

type equation = Eq of typ * typ

(******************************************************************************)
(** type substitution *********************************************************)
(******************************************************************************)

(**
 * These are useful functions for applying a substitution of a type for a type
 * variable
 *)

(** A substitution of a type for a type variable *)
type substitution = talpha * typ

(** apply a type substitution to a type *)
let rec subst_typ ((x,t'):substitution) (t:typ) =
  match t with
  | TAlpha y
      -> if y = x then t' else TAlpha y
  | TUnit | TInt | TBool | TString
      -> t
  | TArrow (t1,t2)
      -> TArrow (subst_typ (x,t') t1, subst_typ (x,t') t2)
  | TStar (t1,t2)
      -> TStar (subst_typ (x,t') t1, subst_typ (x,t') t2)
  | TVariant (ts, name)
      -> TVariant (List.map (subst_typ (x,t')) ts, name)

(** apply a type substitution to a list of equations *)
let subst_eqn (s : substitution) (eqns : equation list) : equation list =
  List.map (fun (Eq (t1,t2)) -> Eq(subst_typ s t1, subst_typ s t2)) eqns

(** apply a type substitution to an annotated expression
    we deliberately violate the 80-column restriction here to make the
    parallelism in the definition clearer, hence easier to read *)
let rec subst_expr (s : substitution) (e : annotated_expr) : annotated_expr =
  match e with
  | AVar      (t,x)            -> AVar      (subst_typ s t, x)
  | AApp      (t,e1,e2)        -> AApp      (subst_typ s t, subst_expr s e1, subst_expr s e2)
  | AFun      (t,(x,tx),e)     -> AFun      (subst_typ s t, (x, subst_typ s tx), subst_expr s e)
  | ALet      (t,(x,tx),e1,e2) -> ALet      (subst_typ s t, (x, subst_typ s tx), subst_expr s e1, subst_expr s e2)
  | ALetRec   (t,(x,tx),e1,e2) -> ALetRec   (subst_typ s t, (x, subst_typ s tx), subst_expr s e1, subst_expr s e2)
  | AUnit     (t)              -> AUnit     (subst_typ s t)
  | AInt      (t,n)            -> AInt      (subst_typ s t, n)
  | ABool     (t,b)            -> ABool     (subst_typ s t, b)
  | AString   (t,k)            -> AString   (subst_typ s t, k)
  | AVariant  (t,c,e)          -> AVariant  (subst_typ s t, c, subst_expr s e)
  | APair     (t,e1,e2)        -> APair     (subst_typ s t, subst_expr s e1, subst_expr s e2)
  | ABinOp    (t,op,e1,e2)     -> ABinOp    (subst_typ s t, op, subst_expr s e1, subst_expr s e2)
  | AIf       (t,e1,e2,e3)     -> AIf       (subst_typ s t, subst_expr s e1, subst_expr s e2, subst_expr s e3)
  | AMatch    (t,e,ps)         -> AMatch    (subst_typ s t, subst_expr s e, List.map (subst_case s) ps)
and subst_case s (p,e) = subst_pat s p, subst_expr s e
and subst_pat  s = function
  | APUnit    (t)              -> APUnit    (subst_typ s t)
  | APInt     (t,n)            -> APInt     (subst_typ s t, n)
  | APBool    (t,b)            -> APBool    (subst_typ s t, b)
  | APString  (t,k)            -> APString  (subst_typ s t, k)
  | APVar     (t,x)            -> APVar     (subst_typ s t, x)
  | APVariant (t,c,p)          -> APVariant (subst_typ s t, c, subst_pat s p)
  | APPair    (t,p1,p2)        -> APPair    (subst_typ s t, subst_pat s p1, subst_pat s p2)

(******************************************************************************)
(** helper functions **********************************************************)
(******************************************************************************)

(* you may find it helpful to implement these or other helper
 * functions, but they are not required.  Feel free to implement them if you
 * need them, change their types or arguments, delete them, whatever.
 *)


(** Format a list of equations for printing. Only beneficial for testing*)
let format_eqns (f : Format.formatter) (eqns : equation list) : unit =
  (* see the comment in Eval.format_value for guidance implementing hints *)
  failwith "unimplemented"

(** use format_eqns to print a value to the console *)
let print_eqns     = Printer.make_printer format_eqns

(** use format_value to convert a value to a string *)
let string_of_eqns = Printer.make_string_of format_eqns



(** generate an unused type variable *)
let newvar () : typ =
     let rec alpha_of_int i =
      let let_of_int i = String.make 1 (char_of_int (i - 1 + int_of_char 'a')) in
      if i <= 0 then "" else (alpha_of_int (i/26))^(let_of_int (i mod 26))
    in

    let next_var  = ref 0 in
    next_var := 1 + !next_var;
    TAlpha (alpha_of_int !next_var)

(*returns the constraints for variants*)
let rec collect_variant (specs:variant_spec list) (t:typ) (c:constructor) (e:typ) =
  match specs with
  | [] -> failwith "Variant for constructor type does not exist"
  | v_spec::tl ->
    let spec_constructors = v_spec.constructors in
    try
     (
      let e_type = List.assoc c spec_constructors in
      let typ_list = List.map (fun elt -> newvar ()) v_spec.vars in
      let v_type = TVariant(typ_list ,v_spec.name) in
      Eq(t,v_type)::Eq(e, e_type)::[]
    )
    with
    | Not_found -> collect_variant tl t c e


(* return the constraints for a binary operator *)
let collect_binop (t:typ) (op:operator) (tl:typ) (tr:typ) : equation list =
  match op with
  | Plus -> Eq(t,TInt)::Eq(tl,TInt)::Eq(tr,TInt)::[]
  | Minus -> Eq(t,TInt)::Eq(tl,TInt)::Eq(tr,TInt)::[]
  | Times -> Eq(t,TInt)::Eq(tl,TInt)::Eq(tr,TInt)::[]
  | Concat -> Eq(t,TString)::Eq(tl,TString)::Eq(tr,TString)::[]
  | _ -> Eq(t,TBool)::Eq(tl,tr)::[]

(** return the constraints for an expr
  * vars refers to a data structure that stores the types of each of the variables
  * that have been defined.
  * It is completely your decision what type of data structure you want to use for vars
  *)
let rec collect_expr (specs:variant_spec list) vars (e : annotated_expr)
                     : equation list =
  match e with
  | AVar(t,v) ->
      (try
        let v_type = List.assoc v vars in
        Eq(t,v_type)::[]
      with
      | Not_found -> failwith ("Unbound var "^v))
  | AApp(t,ae1,ae2) ->
      let t1 = collect_expr specs vars ae1 in
      let t2 = collect_expr specs vars ae2 in
      let type_ae1 = typeof ae1 in
      let type_ae2 = typeof ae2 in
      (Eq(type_ae1,TArrow(type_ae2,t))::[])@t1@t2
  | AFun(t,(v1,t1),ae2)->
      let type_ae2 = collect_expr specs ((v1,t1)::vars) ae2 in
      (Eq(t,TArrow(t1,typeof ae2))::Eq(t1,newvar ())::[])@type_ae2
  | ALet(t,(v1,t1),ae2,ae3) ->
      let type_ae3 = collect_expr specs ((v1,t1)::vars) ae3 in
      (Eq(t,typeof ae3)::Eq(t1,typeof ae2)::[])@type_ae3
  | ALetRec(t,(v1,t1),ae2,ae3) ->
      let type_ae2 = collect_expr specs ((v1,t1)::vars) ae2 in
      let type_ae3 = collect_expr specs ((v1,t1)::vars) ae3 in
      let type_a3 = typeof ae3 in
      (Eq(t,type_a3)::Eq(t1,typeof ae2)::[])@type_ae2@type_ae3
  | AUnit(t) -> Eq(t,TUnit)::[]
  | AInt(t,i) -> Eq(t,TInt)::[]
  | ABool(t,b) -> Eq(t,TBool)::[]
  | AString(t,s) -> Eq(t,TString)::[]
  | AVariant(t,c,ae1) ->
    let e_type_list = collect_expr specs vars ae1 in
    (collect_variant specs t c (typeof ae1))@e_type_list
  | APair(t,ae1,ae2) ->
    let t1 = collect_expr specs vars ae1 in
    let t2 = collect_expr specs vars ae2 in
    (Eq(t, TStar(typeof ae1, typeof ae2))::[])@t1@t2
  | ABinOp(t,op,ae1,ae2) ->
    let binops = collect_binop t op (typeof ae1) (typeof ae2) in
    let t1 = collect_expr specs vars ae1 in
    let t2 = collect_expr specs vars ae2 in
    binops@t1@t2
  | AIf(t,ae1,ae2,ae3) ->
      let t1 = typeof ae1 in
      let t2 = typeof ae2 in
      let t3 = typeof ae3 in
      let list2 = collect_expr specs vars ae2 in
      let list3 = collect_expr specs vars ae3 in
      (Eq(t1, TBool)::Eq(t2,t3)::[])@(list2)@(list3)
  | AMatch(t,ae1,ae_list) ->
      let type_ae1 = typeof ae1 in
      let type_list_ae1 = collect_expr specs vars ae1 in
      let pat_list = List.map (fun (p,_) -> Eq(typeof_pat p, type_ae1)) ae_list in
      let collect_pats_and_exprs =
        fun (pat_list,exp_list) (p,e) ->
          let (eq,variables) = collect_pat specs p in
          let new_vars = if (List.length variables = 0) then vars else variables@vars in
          let collected_exp = collect_expr specs new_vars e in
          (eq@pat_list,collected_exp@exp_list)
      in
      let (collected_pat_list,expr_collect_list) = List.fold_left collect_pats_and_exprs ([],[]) ae_list in
      let expr_list = List.map (fun (_,e) -> Eq(typeof e,t)) ae_list in
      pat_list@collected_pat_list@expr_list@type_list_ae1@expr_collect_list

(** return the constraints for a match cases
  * tconst refers to the type of the parameters of the specific constructors
  * tvariant refers to the type of the variant as a whole
  *)
and collect_case specs vs tconst tvariant ((p:annotated_pattern),(e:annotated_expr)) =
  failwith "unimplemented"

and collect_var_pattern specs t c ap =
  match specs with
  | [] -> failwith "Variant for constructor pattern type does not exist"
  | v_spec::tl ->
    let spec_constructors = v_spec.constructors in
    if List.mem_assoc c spec_constructors then
      let ap_type = List.assoc c spec_constructors in
      let (ap_eq_list, ap_var_list) = collect_pat specs ap in
      let typ_list = List.map (fun elt ->  newvar ()) v_spec.vars in
      let v_type = TVariant(typ_list,v_spec.name) in
      ((Eq(t,v_type)::Eq(typeof_pat ap, ap_type)::[])@ap_eq_list, [])
    else
      collect_var_pattern tl t c ap

(** return the constraints and variables for a pattern *)
(* consider variables in APVar or pair *)
and collect_pat specs (p:annotated_pattern) =
  match p with
  | APUnit(t) -> ([Eq(t,TUnit)], [])
  | APInt(t,i) -> ([Eq(t,TInt)], [])
  | APBool(t,b) -> ([Eq(t,TBool)], [])
  | APString(t,s) -> ([Eq(t,TString)], [])
  | APVar(t,v) -> ([Eq(t,TAlpha("'a"))], [(v,t)])
  | APVariant(t,c,ap) -> collect_var_pattern specs t c ap
  | APPair(t,ap1,ap2) ->
      let (eq_ap1,vars_ap1) = collect_pat specs ap1 in
      let (eq_ap2,vars_ap2) = collect_pat specs ap2 in
      ((Eq(t,TStar(typeof_pat ap1,typeof_pat ap2))::[])@eq_ap1@eq_ap2,
                              vars_ap1@vars_ap2)

(******************************************************************************)
(** constraint generation                                                    **)
(******************************************************************************)

(**
 * collect traverses an expression e and returns a list of equations that must
 * be satisfied for e to typecheck.
 *)
let collect specs e =
  (*create a match with everyone of the different annotated expression types
  use collect_expr because collect isnt recursive. collect should be one line
  which will call collect_expr *)
  collect_expr specs [] e

(******************************************************************************)
(** constraint solver (unification)                                          **)
(******************************************************************************)

let rec occurs_in x = function
  | TAlpha y
      -> x = y
  | TArrow (t1,t2) | TStar (t1,t2)
      -> occurs_in x t1 || occurs_in x t2
  | TVariant (ts,_)
      -> List.exists (occurs_in x) ts
  | TUnit | TInt | TBool | TString
      -> false

(**
 * unify solves a system of equations and returns a list of
 * definitions for the type variables.
 *)
let rec unify eqns = match eqns with
  | [] -> []

  | Eq (t1,t2)::tl when t1 = t2
     -> unify tl

  | Eq ((TAlpha x as t1), (t as t2))::tl
  | Eq ((t as t1), (TAlpha x as t2))::tl
     -> if occurs_in x t
        then failwith (Format.asprintf "circular type constraint %a = %a"
                                       Printer.format_type t1
                                       Printer.format_type t2)
        else (x,t)::(unify (subst_eqn (x,t) tl))

  | Eq (TArrow (t1,t1'), TArrow (t2,t2'))::tl
  | Eq (TStar  (t1,t1'), TStar  (t2,t2'))::tl
     -> unify ((Eq (t1,t2))::(Eq (t1',t2'))::tl)

  | Eq ((TVariant (t1s, n1) as t1), (TVariant (t2s, n2) as t2))::tl
     -> if n1 <> n2
        then failwith (Format.asprintf "can't unify %a and %a"
                                       Printer.format_type t1
                                       Printer.format_type t2)
        else unify ((List.map2 (fun t1 t2 -> Eq (t1,t2)) t1s t2s)
                    @ tl)

  | Eq (t1,t2)::tl
     -> failwith (Format.asprintf "can't unify %a and %a"
                                  Printer.format_type t1
                                  Printer.format_type t2)

(******************************************************************************)
(** inference                                                                **)
(******************************************************************************)

(**
 * rename the type variables so that the first is "a", the
 * second "b", and so on.  Example:
 *
 *  rename_vars ('t23 -> 't17 -> 't23 -> int)
 *  is          ('a   -> 'b   -> 'a   -> int)
 *)
let rec simplify e =
  let rec alpha_of_int i =
    let let_of_int i = String.make 1 (char_of_int (i - 1 + int_of_char 'a')) in
    if i <= 0 then "" else (alpha_of_int (i/26))^(let_of_int (i mod 26))
  in

  let next_var  = ref 0 in

  let newvar () =
    next_var := 1 + !next_var;
    TAlpha (alpha_of_int !next_var)
  in

  let rec subst vars = function
    | TAlpha x -> if List.mem_assoc x vars then vars else (x,newvar())::vars
    | TUnit | TInt | TBool | TString -> vars
    | TArrow (t1,t2) | TStar (t1,t2) -> let vars' = subst vars t1 in
                                        subst vars' t2
    | TVariant (ts,_) -> List.fold_left subst vars ts
  in

  subst [] e

(**
 * given an expression, return the type for that expression,
 * failing if it cannot be typed.
 *)
let infer defs e =
  let annotated = annotate e in
  let eqns      = collect defs annotated in
  let solution  = unify eqns in
  let newtype   = List.fold_left (fun e s -> subst_expr s e) annotated solution in
  let simplify  = simplify (typeof newtype) in
  List.fold_right subst_expr simplify newtype

