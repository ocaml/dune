module Unspecified = Path_intf.Unspecified

val basename_opt : is_root:('a -> bool) -> basename:('a -> 'b) -> 'a -> 'b option
val explode_path : string -> string list
val is_dir_sep : char -> bool

module Local_gen : sig
  include Path_intf.Local_gen

  module Prefix : sig
      type 'w local = 'w t
      type 'w t

      val make : 'w local -> 'w t
      val drop : 'w t -> 'w local -> 'w local option

      (* for all local path p, drop (invalid p = None) *)
      val invalid : 'w t
    end
    with type 'w local := 'w t
end

module Local : sig
  type w = Unspecified.w
  type t = w Local_gen.t

  include Path_intf.S with type t := t
  module Table : Hashtbl.S with type key = t

  val root : t
  val is_root : t -> bool
  val relative : ?error_loc:Loc0.t -> t -> string -> t
  val append : t -> t -> t
  val descendant : t -> of_:t -> t option
  val is_descendant : t -> of_:t -> bool
  val reach : t -> from:t -> string

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
    val relative_result : t -> string list -> (t, [ `Outside_the_workspace ]) Result.t
  end

  val split_first_component : t -> (string * t) option
  val explode : t -> string list
  val of_local : t -> t

  module Prefix : sig
      type local = t
      type t

      val make : local -> t
      val drop : t -> local -> local option

      (* for all local path p, drop (invalid p = None) *)
      val invalid : t
    end
    with type local := t
end
