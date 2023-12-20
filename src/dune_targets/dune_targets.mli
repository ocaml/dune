open Import

(** A non-validated set of targets of a build rule. *)
type t

(** The empty set of targets. Note that rules are not allowed to have the empty
    set of targets, but it is convenient to construct [t] by aggregating several
    sources of information, for some of which it's OK to be empty. *)
val empty : t

val is_empty : t -> bool

(** Combine the sets of file and directory targets. *)
val combine : t -> t -> t

val diff : t -> t -> t
val iter : t -> file:(Path.Build.t -> unit) -> dir:(Path.Build.t -> unit) -> unit

module File : sig
  (** A single file target. *)
  val create : Path.Build.t -> t
end

module Files : sig
  (** A set of file targets. *)
  val create : Path.Build.Set.t -> t
end

(** A set of file and directory targets. *)
val create : files:Path.Build.Set.t -> dirs:Path.Build.Set.t -> t

module Validated : sig
  type unvalidated := t

  (** A rule can produce a set of files whose names are known upfront, as well
      as a set of "opaque" directories whose contents is initially unknown. *)
  type t = private
    { root : Path.Build.t (** [files] and [dirs] are relative to [root] *)
    ; files : Filename.Set.t
    ; dirs : Filename.Set.t
    }

  val iter : t -> file:(Path.Build.t -> unit) -> dir:(Path.Build.t -> unit) -> unit

  val fold
    :  t
    -> init:'acc
    -> file:(Path.Build.t -> 'acc -> 'acc)
    -> dir:(Path.Build.t -> 'acc -> 'acc)
    -> 'acc

  (** If [t] contains at least one file, then it's the lexicographically first
      target file. Otherwise, it's the lexicographically first target directory. *)
  val head : t -> Path.Build.t

  val to_dyn : t -> Dyn.t
  val unvalidate : t -> unvalidated
  val to_trace_args : t -> (string * Chrome_trace.Json.t) list
end

module Validation_result : sig
  type t =
    | Valid of Validated.t
    | No_targets
    | Inconsistent_parent_dir
    | File_and_directory_target_with_the_same_name of Path.Build.t
end

(** Ensure that the set of targets is well-formed. *)
val validate : t -> Validation_result.t

(** Like [Validate.head] but can return [None], because [t] is not guaranteed to
    be non-empty. *)
val head : t -> Path.Build.t option

val to_dyn : t -> Dyn.t
val all : t -> Path.Build.t list

(** The set of targets produced by an action. Each target may be tagged with a
    payload, for example, the target's digest. *)
module Produced : sig
  type 'a t = private
    { root : Path.Build.t (** [files] and [dirs] are relative to [root] *)
    ; files : 'a Filename.Map.t
    ; dirs : 'a Filename.Map.t Path.Local.Map.t
    }

  module Error : sig
    type t

    val message : t -> 'a Pp.t list
    val to_string_hum : t -> string
  end

  (** Expand [targets : Validated.t] by recursively traversing directory targets
      and collecting all contained files. *)
  val of_validated : Validated.t -> (unit t, Error.t) result

  (** Construct from a set of files in the root directory. *)
  val of_files : Path.Build.t -> 'a Path.Local.Map.t -> 'a t

  (** Union of [t.files] and all files in [t.dirs] as [Seq.t] for efficient traversal.
      The resulting [Path.Local.t]s are relative to [t.root]. *)
  val all_files_seq : 'a t -> (Path.Local.t * 'a) Seq.t

  (** Check if a file is present in the targets. *)
  val mem : 'a t -> Path.Build.t -> bool

  (** Find the value associated with the file, if any. *)
  val find : 'a t -> Path.Build.t -> 'a option

  (** Find all files in a directory target or a subdirectory. *)
  val find_dir : 'a t -> Path.Build.t -> 'a Filename.Map.t option

  val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool
  val exists : 'a t -> f:('a -> bool) -> bool
  val foldi : 'a t -> init:'acc -> f:(Path.Local.t -> 'a -> 'acc -> 'acc) -> 'acc
  val iteri : 'a t -> f:(Path.Local.t -> 'a -> unit) -> unit
  val parallel_map : 'a t -> f:(Path.Local.t -> 'a -> 'b Fiber.t) -> 'b t Fiber.t

  (** Aggregate all content digests. *)
  val digest : Digest.t t -> Digest.t

  val map_with_errors
    :  'a t
    -> all_errors:bool
    -> f:(Path.Build.t -> 'a -> ('b, 'e) result)
    -> ('b t, (Path.Build.t * 'e) Nonempty_list.t) result

  val to_dyn : _ t -> Dyn.t
end
