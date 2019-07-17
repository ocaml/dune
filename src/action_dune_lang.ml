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

module Mapper = Action_mapper.Make(Uast)(Uast)

let upgrade_to_dune =
  let id ~dir:_ p = p in
  let dir = String_with_vars.make_text Loc.none "" in
  Mapper.map ~dir ~f_program:id ~f_path:id
    ~f_string:(fun ~dir:_ sw ->
      String_with_vars.upgrade_to_dune sw ~allow_first_dep_var:false)

let encode_and_upgrade a = encode (upgrade_to_dune a)

let remove_locs =
  let dir = String_with_vars.make_text Loc.none "" in
  let f_program ~dir:_ = String_with_vars.remove_locs in
  let f_path ~dir:_ = String_with_vars.remove_locs in
  let f_string ~dir:_ = String_with_vars.remove_locs in
  Mapper.map ~dir ~f_program ~f_path ~f_string

let compare_no_locs t1 t2 = compare (remove_locs t1) (remove_locs t2)

open Dune_lang.Decoder
let decode =
  if_list
    ~then_:decode
    ~else_:
      (loc >>| fun loc ->
       User_error.raise
         ~loc
         [ Pp.textf
             "if you meant for this to be executed with bash, write \
              (bash \"...\") instead"
         ])

let to_dyn a =
  Dune_lang.to_dyn (encode a)
