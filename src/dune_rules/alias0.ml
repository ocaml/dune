open Import

let standard name =
  let name = Alias.Name.of_string name in
  Alias.register_as_standard name;
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
let ocaml_index = standard "ocaml-index"
let runtest = standard "runtest"
let all = standard "all"
