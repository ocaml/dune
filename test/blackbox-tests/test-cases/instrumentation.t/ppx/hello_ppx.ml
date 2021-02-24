open Ast_helper
open Longident

let place = ref None

let file = ref None

let read_file () =
  match !file with
  | None -> "<none>"
  | Some s ->
    let ic = open_in s in
    begin match input_line ic with
    | exception End_of_file ->
      close_in ic;
      "<none>"
    | s ->
      close_in ic;
      s
    end

let impl str =
  let arg =
    match !place with
    | None -> Exp.ident (Location.mknoloc (Lident "__MODULE__"))
    | Some s -> Exp.constant (Const.string (Printf.sprintf "%s (%s)" s (read_file ())))
  in
  Str.eval
    (Exp.apply (Exp.ident (Location.mknoloc (Ldot (Lident "Hello", "hello"))))
       [Nolabel, arg]) :: str

open Ppxlib

let () =
  Driver.add_arg "-place" (Arg.String (fun s -> place := Some s))
    ~doc:"PLACE where to say hello from";
  Driver.add_arg "-file" (Arg.String (fun s -> file := Some s))
    ~doc:"Add info from file"

let () =
  Driver.register_transformation_using_ocaml_current_ast ~impl "hello"
