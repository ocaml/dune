open Ast_helper
open Longident

let place = ref None

let impl str =
  let arg =
    match !place with
    | None -> Exp.ident (Location.mknoloc (Lident "__MODULE__"))
    | Some s -> Exp.constant (Const.string s)
  in
  Str.eval
    (Exp.apply (Exp.ident (Location.mknoloc (Ldot (Lident "Hello", "hello"))))
       [Nolabel, arg]) :: str

open Ppxlib

let () =
  Driver.add_arg "-place" (Arg.String (fun s -> place := Some s))
    ~doc:"PLACE where to say hello from"

let () =
  Driver.register_transformation_using_ocaml_current_ast ~impl "hello"
