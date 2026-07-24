open Import

type t =
  | Ocaml
  | Melange

val equal : t -> t -> bool
val repr : t Repr.t
val to_dyn : t -> Dyn.t
val of_lib_mode : Lib_mode.t -> t
val default_sandbox : Dune_engine.Sandbox_config.t

module Set : sig
  type mode := t
  type t

  val of_lib_mode_set : Lib_mode.Map.Set.t -> t
  val to_list : t -> mode list
  val for_merlin : t -> mode
end

module Per_mode : sig
  type mode := t

  type 'a t =
    { ocaml : 'a
    ; melange : 'a
    }

  val just : 'a -> for_:mode -> 'a option t
  val both : 'a -> 'a t
  val choose : 'a option t -> 'a option
  val from_fun : (for_:mode -> 'a) -> 'a t
  val of_list : (mode * 'a) list -> init:'a -> 'a t
  val to_list : 'a option t -> (mode * 'a) list
  val map : 'a t -> f:(for_:mode -> 'a -> 'b) -> 'b t
  val to_dyn : 'a Dyn.builder -> 'a t Dyn.builder
  val get : for_:mode -> 'a t -> 'a

  module Memo : sig
    val from_fun : (for_:mode -> 'a Memo.t) -> 'a t Memo.t
  end
end
