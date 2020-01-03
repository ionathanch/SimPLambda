open Ast
open Errors

(** [Env] is module to help with environments, which 
    are maps that have strings as keys. *)
module Env = Map.Make(String)

(** [env] is the type of an environment, which maps
    a string to a value. *)
type env = value Env.t

(** [value] is the type of a lambda calculus value.
    In the environment model, that is a closure. *)
and value = 
  | Closure of string * expr * env
  | Int of int
  | Bool of bool

let unbound_var_err = "Unbound variable"

(** [eval env e] is the [<env, e> ==> v] relation. *)
let rec eval (env : env) (e : expr) : value = match e with
  | Var x -> eval_var env x
  | App (e1, e2) -> eval_app env e1 e2
  | Fun (x, e) -> Closure (x, e, env)
  | Let (x, e1, e2) -> eval env (App (Fun (x, e2), e1)) 
  | Int i -> Int i
  | Bool b -> Bool b
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3

(** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
and eval_var env x = 
  try Env.find x env with Not_found -> failwith unbound_var_err

(** [eval_app env e1 e2] is the [v] such that [<env, e1 e2> ==> v]. *)
and eval_app env e1 e2 = 
  match eval env e1 with
  | Closure (x, e, defenv) ->
    let v2 = eval env e2 in
    let env_for_body = Env.add x v2 defenv in
    eval env_for_body e
  | _ -> failwith apply_fun_err

(** [eval_bop bop e1 e2] is the [e] such that [e1 bop e2 ==> e]. *)
and eval_bop env bop e1 e2 = match bop, eval env e1, eval env e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

(** [eval_if e1 e2 e3] is the [e] such that [if e1 then e2 else e3 ==> e]. *)
and eval_if env e1 e2 e3 = match eval env e1 with
  | Bool true -> eval env e2
  | Bool false -> eval env e3
  | _ -> failwith if_guard_err
