open Import

(** In the current workspace (anything under the current project root) *)
module Local : sig
  type t

  val compare : t -> t -> Ordering.t

  module Set : Set.S with type elt = t

  val root : t
  val is_root : t -> bool
  val to_string : t -> string
  val mkdir_p : t -> unit
  val ensure_parent_directory_exists : t -> unit
  val append : t -> t -> t
  val descendant : t -> of_:t -> t option
  val parent : t -> t
end

(** In the outside world *)
module External : sig
  type t

  val to_string : t -> string
end

module Kind : sig
  type t =
    | External of External.t
    | Local    of Local.t
end

type t

val t : t Sexp.Of_sexp.t
val sexp_of_t : t Sexp.To_sexp.t

val compare : t -> t -> Ordering.t
(** a directory is smaller than its descendants *)

module Set : sig
  include Set.S with type elt = t
  val sexp_of_t : t Sexp.To_sexp.t
  val of_string_set : String_set.t -> f:(string -> elt) -> t
end

module Map : Map.S with type key = t


val kind : t -> Kind.t

val of_string : ?error_loc:Loc.t -> string -> t
val to_string : t -> string

(** [to_string_maybe_quoted t] is [maybe_quoted (to_string t)] *)
val to_string_maybe_quoted : t -> string

val root : t
val is_root : t -> bool

val is_local : t -> bool

val relative : ?error_loc:Loc.t -> t -> string -> t

(** Create an external path. If the argument is relative, assume it is
    relative to the initial directory jbuilder was launched in. *)
val absolute : string -> t

(** Convert a path to an absolute filename. Must be called after the
    workspace root has been set. *)
val to_absolute_filename : t -> string

val reach : t -> from:t -> string
val reach_for_running : t -> from:t -> string

val descendant : t -> of_:t -> t option
val is_descendant : t -> of_:t -> bool

val append : t -> t -> t

val basename : t -> string
val parent : t -> t

val extend_basename : t -> suffix:string -> t

(** Extract the build context from a path. For instance, representing paths as strings:

    {[
      extract_build_context "_build/blah/foo/bar" = Some ("blah", "foo/bar")
    ]}
*)
val extract_build_context : t -> (string * t) option

(** Same as [extract_build_context] but return the build context as a path:

    {[
      extract_build_context "_build/blah/foo/bar" = Some ("_build/blah", "foo/bar")
    ]}
*)
val extract_build_context_dir : t -> (t * t) option

(** Drop the "_build/blah" prefix *)
val drop_build_context : t -> t option
val drop_build_context_exn : t -> t

(** Drop the "_build/blah" prefix if present, return [t] otherwise *)
val drop_optional_build_context : t -> t

val explode : t -> string list option
val explode_exn : t -> string list

(** The build directory *)
val build_dir : t

(** [is_in_build_dir t = is_descendant t ~of:build_dir] *)
val is_in_build_dir : t -> bool

(** [is_in_build_dir t = is_local t && not (is_in_build_dir t)] *)
val is_in_source_tree : t -> bool

val is_alias_stamp_file : t -> bool

(**  Split after the first component if [t] is local *)
val split_first_component : t -> (string * t) option

val insert_after_build_dir_exn : t -> string -> t

val exists : t -> bool
val readdir : t -> string list
val is_directory : t -> bool
val rmdir : t -> unit
val unlink : t -> unit
val unlink_no_err : t -> unit
val rm_rf : t -> unit

(** Changes the extension of the filename (or adds an extension if there was none) *)
val change_extension : ext:string -> t -> t

val extension : t -> string

(** maintains the invariant:
    {[
      let suffix = Option.value_exn (Path.drop_prefix t ~prefix) in
      Path.relative prefix suffix = t
    ]}
*)
val drop_prefix : t -> prefix:t -> string option

val pp : t Fmt.t
