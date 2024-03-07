open Import

type t = private
  | Env of Env.Var.t
  | File of Path.t
  | Alias of Alias.t
  | File_selector of File_selector.t
  | Universe

val file : Path.t -> t
val env : Env.Var.t -> t
val universe : t
val file_selector : File_selector.t -> t
val alias : Alias.t -> t
val compare : t -> t -> Ordering.t

module Map : sig
  type dep := t

  include Map.S with type key := t

  val has_universe : _ t -> bool
  val parallel_map : 'a t -> f:(dep -> 'a -> 'b Memo.t) -> 'b t Memo.t
end

module Fact : sig
  (** A fact about the world. For instance:

      - file [p] has digest [d]

      - alias [a] expands to the given set of files, with their digests

      You can think of a [Dep.t] as the "label" of a fact. *)

  type t

  val equal : t -> t -> bool
  val compare : t -> t -> Ordering.t
  val nothing : t
  val file : Path.t -> Digest.t -> t
  val to_dyn : t -> Dyn.t

  module Files : sig
    (** A group of files for which we cache the digest of the whole group. *)
    type t

    (** Record a file-digest mapping for a given [Filename_set.t]. If the set is empty,
        record the parent directory as existing, so that it can be created in the sandbox
        even when the mapping is empty. *)
    val create : Filename_set.t -> build_file:(Path.t -> Digest.t Memo.t) -> t Memo.t

    val to_dyn : t -> Dyn.t
    val equal : t -> t -> bool
  end

  (** [digest] is assumed to be the [digest_paths expansion]. *)
  val alias : Alias.t -> Files.t -> t

  (** [digest] is assumed to be the [digest_paths expansion]. *)
  val file_selector : File_selector.t -> Files.t -> t
end

module Set : sig
  include Set.S with type elt = t and type 'a map := 'a Map.t and type t = unit Map.t

  (** [of_source_files ~files ~empty_directories] depend on all source files
      [files].

      Dependency on a [files] requires special care for empty directories. Empty
      directories need to be loaded so that we clean up stale artifacts in such
      directories. This is why [empty_directories] must be provided *)
  val of_source_files : files:Path.Set.t -> empty_directories:Path.Set.t -> t

  val of_files : Path.t list -> t
  val of_files_set : Path.Set.t -> t
  val encode : t -> Dune_sexp.t
  val add_paths : t -> Path.Set.t -> t
  val digest : t -> Digest.t
end

module Facts : sig
  type dep := t

  (* There is an invariant that is not currently enforced: values correspond to
     keys. For example, we can't have [Map.find (File f) = File_selector _]. *)
  type t = Fact.t Map.t

  val singleton : dep -> Fact.t -> t
  val equal : t -> t -> bool
  val empty : t
  val union : t -> t -> t
  val union_all : t list -> t
  val record_facts : Set.t -> f:(dep -> Fact.t Memo.t) -> t Memo.t

  (** Return all file paths, expanding aliases if [expand_aliases = true]. *)
  val paths : t -> expand_aliases:bool -> Path.Set.t

  (** Create a single [Fact.Files.t] from all the paths contained in a list of
      fact maps. Does so while preserving as much sharing as possible with the
      original [Files.t]. *)
  val group_paths_as_fact_files : t list -> Fact.Files.t

  (** directory targets and parent directories of all dependencies in the build
      directory. Needed for sandboxing *)
  val necessary_dirs_for_sandboxing : t -> Path.Build.Set.t

  val digest : t -> env:Env.t -> Digest.t
  val to_dyn : t -> Dyn.t
end
