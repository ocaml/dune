open Ast_mapper
open Ast_helper
open Longident

let mapper _ _ =
  { default_mapper with
    structure = fun _ str ->
      Str.eval
        (Exp.apply (Exp.ident (Location.mknoloc (Ldot (Lident "Hello", "hello"))))
           [Nolabel, Exp.construct (Location.mknoloc (Lident "()")) None]) :: str
  }

open Migrate_parsetree

let () =
  Driver.register ~name:"hello" Versions.ocaml_current mapper
