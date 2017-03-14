open! Import

type t = Byte | Native

val t : t Sexp.Of_sexp.t

val all : t list

val compiled_unit_ext : t -> string
val compiled_lib_ext : t -> string
val exe_ext : t -> string

val cm_kind : t -> Cm_kind.t
val of_cm_kind : Cm_kind.t -> t

val findlib_predicate : t -> string

module Dict : sig
  type mode = t

  type 'a t =
    { byte   : 'a
    ; native : 'a
    }

  val get : 'a t -> mode -> 'a

  val of_func : (mode:mode -> 'a) -> 'a t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  module Set : sig

    type nonrec t = bool t
    val t : t Sexp.Of_sexp.t
    val all : t
    val is_empty : t -> bool
    val to_list : t -> mode list
    val of_list : mode list -> t
    val iter : t -> f:(mode -> unit) -> unit
  end

  module Binary_Kind_Set : sig

    type binary_kind =
      | Executable
      | Object
      | Shared_object

    type nonrec t = binary_kind list t
    val t : t Sexp.Of_sexp.t
    val default : t
    val all : t
    val is_empty : t -> bool
    val to_list : t -> (mode * binary_kind) list
    val of_list : (mode * binary_kind) list -> t
    val iter : t -> f:((mode * binary_kind) -> unit) -> unit
    val best_executable_mode : t -> mode option
  end


end with type mode := t
