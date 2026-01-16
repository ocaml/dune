open Import

type t =
  | Ocaml
  | Melange

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t

type modes =
  { modes : t list
  ; for_merlin : t
  }

val of_lib_mode : Lib_mode.t -> t
val of_mode_set : Lib_mode.Map.Set.t -> modes

module By_mode : sig
  type mode := t

  type 'a t =
    { ocaml : 'a
    ; melange : 'a
    }

  val just : 'a -> for_:mode -> 'a option t
  val both : 'a -> 'a t
  val from_fun : (for_:mode -> 'a) -> 'a t
  val to_list : 'a option t -> (mode * 'a) list
  val of_list : (mode * 'a) list -> init:'a -> 'a t
  val map : 'a t -> f:(for_:mode -> 'a -> 'b) -> 'b t
  val to_dyn : 'a Dyn.builder -> 'a t Dyn.builder
  val get : for_:mode -> 'a t -> 'a
  val set : for_:mode -> 'a t -> 'a -> 'a t

  module Memo : sig
    val map
      :  'a option t
      -> f:(for_:mode -> 'a option -> 'b option Memo.t)
      -> 'b option t Memo.t

    val from_fun : (for_:mode -> 'a Memo.t) -> 'a t Memo.t
  end
end
