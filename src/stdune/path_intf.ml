module type S = sig
  type t

  val hash : t -> int

  val to_string : t -> string
  val of_string : string -> t
  val parse_string_exn : loc:Loc0.t -> string -> t

  val pp : Format.formatter -> t -> unit

  (** a directory is smaller than its descendants *)
  include Comparable.S with type t := t

  include Comparable.OPS with type t := t

  val to_dyn : t -> Dyn.t
  val to_sexp : t -> Sexp.t

  val extension : t -> string

  (** [set_extension path ~ext] replaces extension of [path] by [ext] *)
  val set_extension : t -> ext:string -> t

  val split_extension : t -> t * string

  val basename : t -> string
  val extend_basename : t -> suffix:string -> t
  val is_suffix : t -> suffix:string -> bool

  module Set : sig
    include Set.S with type elt = t
    val to_sexp : t Sexp.Encoder.t
    val of_listing : dir:elt -> filenames:string list -> t
  end

  module Map :  Map.S with type key = t

  module Table : Hashtbl.S with type key = t

  val relative : ?error_loc:Loc0.t -> t -> string -> t

  val to_string_maybe_quoted : t -> string

  val is_descendant : t -> of_:t -> bool

  val is_root : t -> bool
  val parent_exn : t -> t
  val parent : t -> t option
end
