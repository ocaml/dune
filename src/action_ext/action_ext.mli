open Stdune
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
  end) : sig
  val action : (Path.t, Path.Build.t) S.t -> Dune_engine.Action.t
end
