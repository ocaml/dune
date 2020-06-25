open Ast_helper
open Longident

let impl str =
  Str.eval
    (Exp.apply (Exp.ident (Location.mknoloc (Ldot (Lident "Hello", "hello"))))
       [Nolabel, Exp.construct (Location.mknoloc (Lident "()")) None]) :: str

open Ppxlib

let () =
  Driver.register_transformation_using_ocaml_current_ast ~impl "hello"
