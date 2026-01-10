open Stdune
module Action = Dune_engine.Action
module Done_or_more_deps = Dune_engine.Done_or_more_deps
open Dune_engine.Action.Ext

module Make (S : sig
    type ('path, 'target) t

    val name : string
    val version : int
    val is_useful_to : memoize:bool -> bool
    val encode : ('p, 't) t -> ('p -> Sexp.t) -> ('t -> Sexp.t) -> Sexp.t
    val bimap : ('a, 'b) t -> ('a -> 'x) -> ('b -> 'y) -> ('x, 'y) t

    val action
      :  (Path.t, Path.Build.t) t
      -> ectx:Exec.context
      -> eenv:Exec.env
      -> unit Fiber.t
  end) =
struct
  module Spec = struct
    include S

    let is_dynamic = false

    let encode t f g =
      let open Sexp in
      List [ Atom name; Atom (Int.to_string version); S.encode t f g ]
    ;;

    let action a ~ectx ~eenv =
      let open Fiber.O in
      let start = Time.now () in
      Dune_trace.emit Action (fun () -> Dune_trace.Event.Action.start ~name ~start);
      let+ () = action a ~ectx ~eenv in
      Dune_trace.emit Action (fun () -> Dune_trace.Event.Action.finish ~name ~start);
      Done_or_more_deps.Done
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
