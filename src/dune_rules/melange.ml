open Import
open Dune_lang.Decoder

let syntax =
  Dune_lang.Syntax.create ~name:"melange" ~desc:"support for Melange compiler"
    [ ((0, 1), `Since (3, 6)) ]

let extension_key =
  Dune_project.Extension.register syntax (return ((), [])) Unit.to_dyn

let js_ext = ".js"

module Module_system = struct
  type t =
    | Es6
    | CommonJs

  let to_string = function
    | Es6 -> "es6"
    | CommonJs -> "commonjs"
end

module Cm_kind = struct
  type t =
    | Cmi
    | Cmj

  let source = function
    | Cmi -> Ocaml.Ml_kind.Intf
    | Cmj -> Impl

  let ext = function
    | Cmi -> ".cmi"
    | Cmj -> ".cmj"

  let to_dyn =
    let open Dyn in
    function
    | Cmi -> variant "cmi" []
    | Cmj -> variant "cmj" []

  module Map = struct
    type 'a t =
      { cmi : 'a
      ; cmj : 'a
      }

    let make_all x = { cmi = x; cmj = x }
  end
end
