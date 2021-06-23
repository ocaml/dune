open Ppxlib

let () =
  Driver.register_transformation_using_ocaml_current_ast ~impl:Fun.id "trivial"
