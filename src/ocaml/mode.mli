open! Stdune

type t =
  | Byte
  | Native

val equal : t -> t -> bool

val compare : t -> t -> Ordering.t

val encode : t -> Dune_sexp.t

val decode : t Dune_sexp.Decoder.t

val all : t list

val compiled_unit_ext : t -> string

val compiled_lib_ext : t -> string

val exe_ext : t -> string

val plugin_ext : t -> string

val cm_kind : t -> Cm_kind.t

val of_cm_kind : Cm_kind.t -> t

val variant : t -> Variant.t

val to_string : t -> string

val to_dyn : t -> Dyn.t

module Dict : sig
  type mode = t

  type 'a t =
    { byte : 'a
    ; native : 'a
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val for_all : 'a t -> f:('a -> bool) -> bool

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

  module List : sig
    type 'a dict

    type 'a t = 'a list dict

    val empty : 'a t

    val decode : 'a Dune_sexp.Decoder.t -> 'a t Dune_sexp.Decoder.t

    val encode : 'a Dune_sexp.Encoder.t -> 'a t -> Dune_sexp.t list
  end
  with type 'a dict := 'a t

  val get : 'a t -> mode -> 'a

  val of_func : (mode:mode -> 'a) -> 'a t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val mapi : 'a t -> f:(mode -> 'a -> 'b) -> 'b t

  val iteri : 'a t -> f:(mode -> 'a -> unit) -> unit

  val foldi : 'a t -> init:'b -> f:(mode -> 'a -> 'b -> 'b) -> 'b

  val make_both : 'a -> 'a t

  val make : byte:'a -> native:'a -> 'a t

  module Set : sig
    type nonrec t = bool t

    val to_dyn : t -> Dyn.t

    val encode : t -> Dune_sexp.t list

    val equal : t -> t -> bool

    val all : t

    val is_empty : t -> bool

    val to_list : t -> mode list

    val of_list : mode list -> t

    val iter_concurrently : t -> f:(mode -> unit Memo.t) -> unit Memo.t
  end
end
with type mode := t

(** [Select] is a utility module that represents a mode selection. *)
module Select : sig
  type mode = t

  type nonrec t =
    | Only of t
    | All

  include Dune_sexp.Conv.S with type t := t

  val of_option : mode option -> t

  val equal : t -> t -> bool

  val is_not_all : t -> bool
end

(** [Map] is a data-structure that can store values that are indexed by keys of
    the type [Select.t]. The key [Select.All] is meant to store values that
    apply to any mode while keys of the form [Select.Only _] designate values
    that apply to specific modes. *)
module Map : sig
  type mode = t

  include Map.S with type key = Select.t

  module Multi : sig
    include module type of Multi

    (** Creates an new map and populate the [All] key with the given list *)
    val create_for_all_modes : 'a list -> 'a t

    (** Returns the list of values associated to the [All] key. *)
    val for_all_modes : 'a t -> 'a list

    (** Returns the list of values associated to a specific mode. If the
        [and_all] option (which defaults to [false]) is set to true then values
        which are not associated to a specific mode are also returned. *)
    val for_only : ?and_all:bool -> 'a t -> mode -> 'a list
  end

  val encode : ('a -> Dune_sexp.t) -> 'a Multi.t -> Dune_sexp.t list

  val decode : 'a Dune_sexp.Decoder.t -> 'a Multi.t Dune_sexp.Decoder.t
end
