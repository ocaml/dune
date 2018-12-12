open! Stdune

type program = String_with_vars.t
type string = String_with_vars.t
type path = String_with_vars.t

module type Uast = Action_intf.Ast
  with type program = String_with_vars.t
  with type path    = String_with_vars.t
  with type string  = String_with_vars.t
module rec Uast : Uast = Uast
include Action_ast.Make(String_with_vars)(String_with_vars)(String_with_vars)(Uast)

module Mapper = Action.Make_mapper(Uast)(Uast)

let upgrade_to_dune =
  let id ~dir:_ p = p in
  let dir = String_with_vars.make_text Loc.none "" in
  Mapper.map ~dir ~f_program:id ~f_path:id
    ~f_string:(fun ~dir:_ -> String_with_vars.upgrade_to_dune)

let encode_and_upgrade a = encode (upgrade_to_dune a)

open Dune_lang.Decoder
let decode =
  if_list
    ~then_:decode
    ~else_:
      (loc >>| fun loc ->
       of_sexp_errorf
         loc
         "if you meant for this to be executed with bash, write (bash \"...\") instead")
