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
    | Cmi -> Filename.Extension.cmi
    | Cmj -> Filename.Extension.cmj
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

module Source = struct
  let extension_prefix = ".melange"
end

let syntax =
  let supported_versions =
    [ (0, 1), `Since (3, 8); (0, 1), `Deleted_in (3, 25); (1, 0), `Since (3, 20) ]
  in
  Syntax.create
    ~name:(Syntax.Name.parse "melange")
    ~desc:"the Melange extension"
    supported_versions
;;
