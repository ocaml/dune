open Import

module Alias_status : sig
  type t =
    | Defined
    | Not_defined

  include Monoid.S with type t := t
end

val alias : Alias.t -> unit Action_builder.t

module Alias_build_info : sig
  (** When looking up aliases, [alias_status] represents whether the alias was
      defined in that directory or not.

      For custom traversals, [allowed_build_only_subdirs] provides additional
      information about immediate, build-only (not present in source)
      sub-directories that the traversal might want to look under. *)
  type t =
    { alias_status : Alias_status.t
    ; allowed_build_only_subdirs : Filename.Set.t
    }
end

module Alias_rec (_ : sig
    (* This API isn't fully baked yet. We might move it to the rules *)

    (** [traverse dir ~f] traverses [dir] and evaluates [f] for every directory.
        Returns [Defined] if [f] returned [Defined] at least once. [Not_defined]
        otherwise. *)
    val traverse
      :  Path.Build.t
      -> f:(path:Path.Build.t -> Alias_build_info.t Action_builder.t)
      -> Alias_status.t Action_builder.t
  end) : sig
  (** Depend on an alias recursively. Return [Defined] if the alias is defined
      in at least one directory, and [Not_defined] otherwise. *)
  val dep_on_alias_rec : Alias.Name.t -> Path.Build.t -> Alias_status.t Action_builder.t
end
