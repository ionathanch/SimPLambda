open Errors
open Ast
open Collect
open Unify
open Eval

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let infer (s : string) : typevar =
  let e = parse s in
  let cstr, t = collect [] [] e in
  unify cstr t

let interp (s : string) : value =
  let e = parse s in
  eval Env.empty e
