open Errors
open Ast

type typevar =
  | TInt
  | TBool
  | TFun of typevar * typevar
  | TypeVar of int

let rec pp_typevar fmt =
  let open Format in
  function
  | TInt -> pp_print_string fmt "Int"
  | TBool -> pp_print_string fmt "Bool"
  | TFun (t1, t2) ->
    pp_print_char fmt '(';
    pp_typevar fmt t1;
    pp_print_space fmt ();
    pp_print_string fmt "->";
    pp_print_space fmt ();
    pp_typevar fmt t2;
    pp_print_char fmt ')'
  | TypeVar i ->
    pp_print_char fmt 'T';
    pp_print_int fmt i

let next_typevar =
  let count = ref 0 in
  let next () = incr count; TypeVar !count in
  next

type context = (string * typevar) list
type constraints = (typevar * typevar) list

let rec collect (ctx : context) (cstr : constraints) (e : expr) : constraints * typevar =
  match e with
  | Int _ -> cstr, TInt
  | Bool _ -> cstr, TBool
  | Var x -> cstr, List.assoc x ctx
  | Binop (bop, e1, e2) -> collect_binop ctx cstr bop e1 e2
  | If (e1, e2, e3) -> collect_if ctx cstr e1 e2 e3
  | App (e1, e2) -> collect_app ctx cstr e1 e2
  | Fun (x, e) -> collect_fun ctx cstr x e
  | Let (x, e1, e2) -> collect_let ctx cstr x e1 e2

and collect_binop ctx cstr bop e1 e2 =
  let cstr, t1 = collect ctx cstr e1 in
  let cstr, t2 = collect ctx cstr e2 in
  let cstr = (t1, TInt) :: cstr in
  let cstr = (t2, TInt) :: cstr in
  let t = match bop with
    | Leq -> TBool
    | _ -> TInt in
  cstr, t

and collect_if ctx cstr e1 e2 e3 =
  let t = next_typevar () in
  let cstr, t1 = collect ctx cstr e1 in
  let cstr, t2 = collect ctx cstr e2 in
  let cstr, t3 = collect ctx cstr e3 in
  let cstr = (t1, TBool) :: cstr in
  let cstr = (t2, t) :: cstr in
  let cstr = (t3, t) :: cstr in
  cstr, t

and collect_app ctx cstr e1 e2 =
  let tr = next_typevar () in
  let cstr, tf = collect ctx cstr e1 in
  let cstr, ta = collect ctx cstr e2 in
  let t = TFun (ta, tr) in
  (t, tf) :: cstr, tr

and collect_fun ctx cstr x e =
  let ta = next_typevar () in
  let ctx = (x, ta) :: ctx in
  let cstr, tr = collect ctx cstr e in
  cstr, TFun (ta, tr)

and collect_let ctx cstr x e1 e2 =
  let cstr, t1 = collect ctx cstr e1 in
  let ctx = (x, t1) :: ctx in
  collect ctx cstr e2
