open Import

(* This mutable table is safe: it's modified only at the top level. *)
let standard_aliases = Table.create (module Dune_engine.Alias.Name) 7
let is_standard name = Table.mem standard_aliases name

let register_as_standard name =
  let (_ : (unit, _) result) = Table.add standard_aliases name () in
  ()
;;

let standard name =
  let name = Alias.Name.of_string name in
  register_as_standard name;
  name
;;

let fmt = standard "fmt"
let lint = standard "lint"
let private_doc = standard "doc-private"
let doc = standard "doc"
let doc_json = standard "doc-json"
let doc_new = standard "doc-new"
let check = standard "check"
let install = standard "install"
let pkg_install = Alias.Name.of_string "pkg-install"
let pkg_lock = Alias.Name.of_string "pkg-lock"
let pkg_tmp_ocamlformat_lock = Alias.Name.of_string "pkg-tmp-ocamlformat-lock"
let ocaml_index = standard "ocaml-index"
let runtest = standard "runtest"
let all = standard "all"
let default = standard "default"
let empty = standard "empty"
