open Collect

type subst = int * typevar
type substs = subst list

let rec substitute (subst : subst) (t : typevar) : typevar =
  match t with
  | TBool -> TBool
  | TInt -> TInt
  | TFun (t1, t2) -> TFun (substitute subst t1, substitute subst t2)
  | TypeVar i -> if i = fst subst then snd subst else t

let rec typevar_in (i : int) (t : typevar) : bool =
  match t with
  | TBool | TInt -> false
  | TFun (t1, t2) -> typevar_in i t1 || typevar_in i t2
  | TypeVar j -> i = j

let subst_in_cstr (subst : subst) (cstr : constraints) : constraints =
  List.map (function (t1, t2) -> substitute subst t1, substitute subst t2) cstr

let rec substs_of_cstr (cstr : constraints) : substs =
  match cstr with
  | [] -> []
  | (t1, t2) :: cstr ->
    match t1, t2 with
    | TypeVar i, t | t, TypeVar i when not (typevar_in i t) ->
      let subst = (i, t) in
      subst :: (substs_of_cstr (subst_in_cstr subst cstr))
    | TFun (t1, t2), TFun (t1', t2') ->
      substs_of_cstr ((t1, t1') :: (t2, t2') :: cstr)
    | _ when t1 = t2 -> substs_of_cstr cstr
    | _ -> failwith "Could not unify type variables."

let unify (cstr : constraints) (t : typevar) : typevar =
  let substs = substs_of_cstr cstr in
  List.fold_right substitute substs t