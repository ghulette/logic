(*
Compile:
ocamlfind ocamlc -package compiler-libs.common -linkpkg -o ppx_qq ppx_qq.ml

Use:
ocamlc -ppx ./ppx_qq example.ml
*)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

let formula_expr loc s =
  let f = "Formula.of_string" in
  let txt = Longident.parse f in
  Exp.apply (Exp.ident {txt; loc}) ["", Exp.constant (Const_string (s, None))]
       
let qq_mapper argv =
  let expr mapper e =
    match e.pexp_desc with
    | Pexp_constant (Const_string (s, Some "p")) ->
       formula_expr e.pexp_loc s
    | _ -> default_mapper.expr mapper e
  in
  { default_mapper with expr }

let () =
  register "qq" qq_mapper
