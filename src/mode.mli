open! Import

type t = Byte | Native

val all : t list

val compiled_unit_ext : t -> string
val compiled_lib_ext : t -> string
val exe_ext : t -> string
val compiler : t -> Context.t -> Path.t option

val cm_kind : t -> Cm_kind.t

val findlib_predicate : t -> string

val best : Context.t -> t

module Dict : sig
  type mode = t

  type 'a t =
    { byte   : 'a
    ; native : 'a
    }

  val get : 'a t -> mode -> 'a

  val of_func : (mode:mode -> 'a) -> 'a t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
end with type mode := t
