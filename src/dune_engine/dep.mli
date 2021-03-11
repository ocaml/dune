open Stdune

type t = private
  | Env of Env.Var.t
  | File of Path.t
  | Alias of Alias.t
  | File_selector of File_selector.t
  | Universe
  | Sandbox_config of Sandbox_config.t

val file : Path.t -> t

val env : Env.Var.t -> t

val universe : t

val file_selector : File_selector.t -> t

val alias : Alias.t -> t

val sandbox_config : Sandbox_config.t -> t

val compare : t -> t -> Ordering.t

module Map : sig
  type dep := t

  include Map.S with type key := t

  val sandbox_config : _ t -> Sandbox_config.t

  val has_universe : _ t -> bool

  val parallel_map :
    'a t -> f:(dep -> 'a -> 'b Memo.Build.t) -> 'b t Memo.Build.t
end

module Fact : sig
  (** A fact about the world. For instance:

      - file [p] has digest [d]

      - alias [a] expands to the given set of files, with their digests

      You can think of a [Dep.t] as the "label" of a fact. *)

  type t

  val nothing : t

  val file : Path.t -> Digest.t -> t

  val alias : Alias.t -> Digest.t Path.Map.t -> t

  val file_selector : File_selector.t -> Digest.t Path.Map.t -> t
end

module Facts : sig
  (* There is an invariant that is not currently enforced: the value correspond
     to the key. For instance we can't have [Map.find (File f) = File_selector
     _] *)
  type t = Fact.t Map.t

  val empty : t

  val union : t -> t -> t

  (** Return all the paths, expanding aliases *)
  val paths : t -> Digest.t Path.Map.t

  val dirs : t -> Path.Set.t

  val digest : t -> sandbox_mode:Sandbox_mode.t -> env:Env.t -> Digest.t
end

module Set : sig
  type dep := t

  type t = unit Map.t

  val empty : t

  val singleton : dep -> t

  val add : t -> dep -> t

  val union : t -> t -> t

  val union_map : 'a list -> f:('a -> t) -> t

  val to_list : t -> dep list

  val of_list : dep list -> t

  val of_list_map : 'a list -> f:('a -> dep) -> t

  (** Return dependencies on all source files under a certain source directory.

      Dependency on a source_tree requires special care for empty directory, so
      you should use this function rather than manually traverse the source
      tree. *)
  val source_tree : Path.t -> t

  (** Same as [source_tree] but also return the set of files as a set. Because
      of the special care for empty directories, the set of dependencies
      returned contains dependencies other than [File]. So exracting the set of
      files from the dependency set is a bit awkward. This is why this function
      exist. *)
  val source_tree_with_file_set : Path.t -> t * Path.Set.t

  val of_files : Path.t list -> t

  val of_files_set : Path.Set.t -> t

  val encode : t -> Dune_lang.t

  val add_paths : t -> Path.Set.t -> t
end
