module type S = sig
  type t

  val hash : t -> int

  val to_string : t -> string

  val of_string : string -> t

  val parse_string_exn : loc:Loc0.t -> string -> t

  (** a directory is smaller than its descendants *)
  include Comparator.S with type t := t

  include Comparator.OPS with type t := t

  val to_dyn : t -> Dyn.t

  val extension : t -> string

  (** [set_extension path ~ext] replaces extension of [path] by [ext] *)
  val set_extension : t -> ext:string -> t

  val split_extension : t -> t * string

  val basename : t -> string

  val basename_opt : t -> string option

  val extend_basename : t -> suffix:string -> t

  module Set : sig
    include Set.S with type elt = t

    val to_dyn : t Dyn.Encoder.t

    val of_listing : dir:elt -> filenames:string list -> t
  end

  module Map : Map.S with type key = t

  module Table : Hashtbl.S with type key = t

  val relative : ?error_loc:Loc0.t -> t -> string -> t

  val to_string_maybe_quoted : t -> string

  val is_descendant : t -> of_:t -> bool

  val is_root : t -> bool

  val parent_exn : t -> t

  val parent : t -> t option
end

(** [Unspecified.w] is a type-level placeholder of an unspecified path. (see
    [Local_gen] for how it's used) *)
module Unspecified = struct
  type w
end

(** ['w Local_gen.t] is the type of local paths that live under ['w]. If
    [x : w Local_gen.t] and [w] is a type-level witness corresponding to a (real
    or hypothetical) filesystem location [base], then we think of [x] as
    referring to the location [to_string base ^/ to_string x]. *)
module type Local_gen = sig
  type 'w t

  val hash : 'w t -> int

  (* it's not clear that these should be polymorphic over 'w, maybe they should
     additionally ask for an object that fixes 'w *)
  val to_string : 'w t -> string

  val of_string : string -> 'w t

  val parse_string_exn : loc:Loc0.t -> string -> 'w t

  (** a directory is smaller than its descendants *)
  val compare : 'w t -> 'w t -> Ordering.t

  val to_dyn : 'w t -> Dyn.t

  val extension : 'w t -> string

  (** [set_extension path ~ext] replaces extension of [path] by [ext] *)
  val set_extension : 'w t -> ext:string -> 'w t

  val split_extension : 'w t -> 'w t * string

  val basename : 'w t -> string

  val extend_basename : 'w t -> suffix:string -> 'w t

  module Fix_root (Root : sig
    type w
  end) : sig
    module Set : sig
      include Set.S with type elt = Root.w t

      val to_dyn : t Dyn.Encoder.t

      val of_listing : dir:elt -> filenames:string list -> t
    end

    module Map : Map.S with type key = Root.w t

    module Table : Hashtbl.S with type key = Root.w t
  end

  val relative : ?error_loc:Loc0.t -> 'w t -> string -> 'w t

  val to_string_maybe_quoted : 'w t -> string

  val is_descendant : 'w t -> of_:'w t -> bool

  val is_root : 'w t -> bool

  val parent_exn : 'w t -> 'w t

  val parent : 'w t -> 'w t option

  val explode : 'w t -> string list

  val root : 'w t

  val append : 'w t -> Unspecified.w t -> 'w t

  val descendant : 'w t -> of_:'w t -> Unspecified.w t option

  val reach : 'w t -> from:'w t -> string

  val split_first_component : 'w t -> (string * Unspecified.w t) option

  module L : sig
    val relative : ?error_loc:Loc0.t -> 'w t -> string list -> 'w t
  end
end
