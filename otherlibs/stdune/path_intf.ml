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

  (** [map_extension path ~f] replaces extension of [path] by [f extension]*)
  val map_extension : t -> f:(string -> string) -> t

  val split_extension : t -> t * string

  val basename : t -> Filename.t

  val basename_opt : t -> Filename.t option

  val extend_basename : t -> suffix:Filename.t -> t

  module Map : Map.S with type key = t

  module Set : sig
    include Set.S with type elt = t and type 'a map = 'a Map.t

    val to_dyn : t Dyn.builder

    val of_listing : dir:elt -> filenames:string list -> t
  end

  module Table : Hashtbl.S with type key = t

  val relative : ?error_loc:Loc0.t -> t -> string -> t

  val to_string_maybe_quoted : t -> string

  val is_descendant : t -> of_:t -> bool

  val is_root : t -> bool

  val parent_exn : t -> t

  val parent : t -> t option

  val unlink_no_err : t -> unit
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

  (** [map_extension path ~f] replaces extension of [path] by [f extension]*)
  val map_extension : 'W t -> f:(string -> string) -> 'W t

  val split_extension : 'w t -> 'w t * string

  val basename : 'w t -> Filename.t

  val extend_basename : 'w t -> suffix:Filename.t -> 'w t

  module Fix_root (Root : sig
    type w
  end) : sig
    module Map : Map.S with type key = Root.w t

    module Set : sig
      include Set.S with type elt = Root.w t and type 'a map = 'a Map.t

      val to_dyn : t Dyn.builder

      val of_listing : dir:elt -> filenames:Filename.t list -> t
    end

    module Table : Hashtbl.S with type key = Root.w t
  end

  val relative : ?error_loc:Loc0.t -> 'w t -> string -> 'w t

  val to_string_maybe_quoted : 'w t -> string

  val is_descendant : 'w t -> of_:'w t -> bool

  val is_root : 'w t -> bool

  val parent_exn : 'w t -> 'w t

  val parent : 'w t -> 'w t option

  val explode : 'w t -> Filename.t list

  val root : 'w t

  val append : 'w t -> Unspecified.w t -> 'w t

  val descendant : 'w t -> of_:'w t -> Unspecified.w t option

  val reach : 'w t -> from:'w t -> string

  val split_first_component : 'w t -> (Filename.t * Unspecified.w t) option

  module L : sig
    val relative : ?error_loc:Loc0.t -> 'w t -> string list -> 'w t

    val relative_result :
      'w t -> string list -> ('w t, [ `Outside_the_workspace ]) Result.t
  end

  val unlink_no_err : 'w t -> unit
end
