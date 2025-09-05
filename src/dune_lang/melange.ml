open Import

module Cm_kind = struct
  type t =
    | Cmi
    | Cmj

  let source = function
    | Cmi -> Ocaml.Ml_kind.Intf
    | Cmj -> Impl
  ;;

  let ext = function
    | Cmi -> ".cmi"
    | Cmj -> ".cmj"
  ;;

  let to_dyn =
    let open Dyn in
    function
    | Cmi -> variant "cmi" []
    | Cmj -> variant "cmj" []
  ;;

  module Map = struct
    type 'a t =
      { cmi : 'a
      ; cmj : 'a
      }

    let make_all x = { cmi = x; cmj = x }
  end
end

let syntax =
  Syntax.create
    ~name:Dune_project.Melange_syntax.name
    ~desc:"the Melange extension"
    [ (0, 1), `Since (3, 8); (1, 0), `Since (3, 20) ]
;;
