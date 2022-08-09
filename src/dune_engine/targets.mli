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
  (** A rule can produce a set of files whose names are known upfront, as well
      as a set of "opaque" directories whose contents is initially unknown. *)
  type t = private
    { files : Path.Build.Set.t
    ; dirs : Path.Build.Set.t
    }

  (** If [t] contains at least one file, then it's the lexicographically first
      target file. Otherwise, it's the lexicographically first target directory. *)
  val head : t -> Path.Build.t

  val to_dyn : t -> Dyn.t
end

module Validation_result : sig
  type t =
    | Valid of
        { parent_dir : Path.Build.t
        ; targets : Validated.t
        }
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

val pp : t -> _ Pp.t

(** The set of targets produced by an action. Each target may be tagged with a
    payload, for example, the target's digest. *)
module Produced : sig
  type 'a t = private
    { files : 'a Path.Build.Map.t
    ; dirs : 'a String.Map.t Path.Build.Map.t
    }

  (** Expand [targets : Validated.t] by recursively traversing directory targets
      and collecting all contained files. *)
  val of_validated :
       Validated.t
    -> (unit t, [ `Directory of Path.Build.t ] * Unix_error.Detailed.t) result

  (** Like [of_validated] but assumes the targets have been just produced by a
      rule. If some directory targets aren't readable, an error is raised *)
  val produced_after_rule_executed_exn : loc:Loc.t -> Validated.t -> unit t

  (** Populates only the [files] field, leaving [dirs] empty. Raises a code
      error if the list contains duplicates. *)
  val of_file_list_exn : (Path.Build.t * Digest.t) list -> Digest.t t

  (** Add a list of discovered directory-filename pairs to [Validated.t]. Raises
      a code error on an unexpected directory. *)
  val expand_validated_exn :
    Validated.t -> (Path.Build.t * string) list -> unit t

  (** Union of [t.files] and all files in [t.dirs]. *)
  val all_files : 'a t -> 'a Path.Build.Map.t

  (** Like [all_files] but returns a [Seq.t] for efficient traversal. *)
  val all_files_seq : 'a t -> (Path.Build.t * 'a) Seq.t

  (** Aggregate all content digests. *)
  val digest : Digest.t t -> Digest.t

  module Option : sig
    val mapi : 'a t -> f:(Path.Build.t -> 'a -> 'b option) -> 'b t option
  end

  val to_dyn : _ t -> Dyn.t
end
