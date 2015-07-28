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

let s_to_i s =
  int_of_string (String.trim s)
         
let qq_mapper argv =
  let expr mapper e =
    match e.pexp_desc with
    | Pexp_constant (Const_string (s, Some "p")) ->
       let loc = e.pexp_loc in
       begin
         try
           let n = s_to_i s in
           Exp.constant ~loc (Const_int n)
         with
           Failure msg -> raise Location.(Error (error ~loc msg))
       end
    | _ -> default_mapper.expr mapper e
  in
  { default_mapper with expr }

let () =
  register "qq" qq_mapper
