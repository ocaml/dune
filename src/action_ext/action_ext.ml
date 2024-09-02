open Stdune
module Action = Dune_engine.Action

module Make (S : Action.Ext.Spec) = struct
  module Spec = struct
    include S

    let encode t f g =
      let open Sexp in
      List [ Atom name; Atom (Int.to_string version); S.encode t f g ]
    ;;
  end

  let action p =
    let module M = struct
      type path = Path.t
      type target = Path.Build.t

      module Spec = Spec

      let v = p
    end
    in
    Action.Extension (module M)
  ;;
end
