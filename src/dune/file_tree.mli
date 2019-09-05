(** Dune representation of the source tree *)
open! Stdune

open! Import

module Dune_file : sig
  module Plain : sig
    (** [sexps] is mutable as we get rid of the S-expressions once they have
        been parsed, in order to release the memory as soon as we don't need
        them. *)
    type t =
      { path : Path.Source.t
      ; mutable sexps : Dune_lang.Ast.t list
      }
  end

  type t = private
    | Plain of Plain.t
    | Ocaml_script of Path.Source.t

  val path : t -> Path.Source.t
end

module Dir : sig
  type t

  val path : t -> Path.Source.t

  val files : t -> String.Set.t

  val file_paths : t -> Path.Source.Set.t

  val sub_dirs : t -> t String.Map.t

  val sub_dir_paths : t -> Path.Source.Set.t

  val sub_dir_names : t -> String.Set.t

  (** Whether this directory is ignored by an [ignored_subdirs] stanza in one
      of its ancestor directories. *)
  val ignored : t -> bool

  (** Whether this directory is vendored or sits within a vendored directory *)
  val vendored : t -> bool

  val vcs : t -> Vcs.t option

  val fold :
    t -> traverse:Sub_dirs.Status.Set.t -> init:'a -> f:(t -> 'a -> 'a) -> 'a

  (** Return the contents of the dune (or jbuild) file in this directory *)
  val dune_file : t -> Dune_file.t option

  (** Return the project this directory is part of *)
  val project : t -> Dune_project.t

  val to_dyn : t -> Dyn.t
end

(** A [t] value represent a view of the source tree. It is lazily constructed
    by scanning the file system and interpreting a few stanzas in [dune] files. *)
type t

val load :
     Path.Source.t
  -> ancestor_vcs:Vcs.t option
  -> recognize_jbuilder_projects:bool
  -> t

(** Passing [~traverse_data_only_dirs:true] to this functions causes the whole
    source tree to be deeply scanned, including ignored sub-trees. *)
val fold :
  t -> traverse:Sub_dirs.Status.Set.t -> init:'a -> f:(Dir.t -> 'a -> 'a) -> 'a

val root : t -> Dir.t

val find_dir : t -> Path.Source.t -> Dir.t option

(** [nearest_dir t fn] returns the directory with the longest path that is an
    ancestor of [fn]. *)
val nearest_dir : t -> Path.Source.t -> Dir.t

(** [nearest_vcs t fn] returns the version control system with the longest root
    path that is an ancestor of [fn]. *)
val nearest_vcs : t -> Path.Source.t -> Vcs.t option

val files_of : t -> Path.Source.t -> Path.Source.Set.t

(** [true] iff the path is a directory *)
val dir_exists : t -> Path.Source.t -> bool

(** [dir_is_vendored t path] tells whether [path] is a vendored directory.
    Returns [None] if it doesn't describe a directory within [t]. *)
val dir_is_vendored : t -> Path.Source.t -> bool option

(** [true] iff the path is a file *)
val file_exists : t -> Path.Source.t -> bool
