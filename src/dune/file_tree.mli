(** Dune representation of the source tree *)
open! Stdune

open! Import

module Dune_file : sig
  module Plain : sig
    (** [sexps] is mutable as we get rid of the S-expressions once they have
        been parsed, in order to release the memory as soon as we don't need
        them. *)
    type t

    val get_sexp_and_destroy : t -> Dune_lang.Ast.t list
  end

  val jbuild_fname : string

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

  val fold_sub_dirs : t -> init:'a -> f:(string -> t -> 'a -> 'a) -> 'a

  val sub_dir_paths : t -> Path.Source.Set.t

  val sub_dir_names : t -> String.Set.t

  val vcs : t -> Vcs.t option

  val status : t -> Sub_dirs.Status.t

  val fold :
    t -> traverse:Sub_dirs.Status.Set.t -> init:'a -> f:(t -> 'a -> 'a) -> 'a

  (** Return the contents of the dune (or jbuild) file in this directory *)
  val dune_file : t -> Dune_file.t option

  (** Return the project this directory is part of *)
  val project : t -> Dune_project.t

  val to_dyn : t -> Dyn.t
end

(** [set source ~ancestor_vcs ~recognize_jbuilder_projects] set the root, the
    default VCS, and if jbuilder project will be recognized. It must be called
    before all other calls to the file tree. All of these settings can only be
    set once per dune process *)
val init : ancestor_vcs:Vcs.t option -> recognize_jbuilder_projects:bool -> unit

val root : unit -> Dir.t

(** Traverse starting from the root and report progress in the status line *)
val fold_with_progress :
  traverse:Sub_dirs.Status.Set.t -> init:'a -> f:(Dir.t -> 'a -> 'a) -> 'a

val find_dir : Path.Source.t -> Dir.t option

(** [nearest_dir t fn] returns the directory with the longest path that is an
    ancestor of [fn]. *)
val nearest_dir : Path.Source.t -> Dir.t

(** [nearest_vcs t fn] returns the version control system with the longest root
    path that is an ancestor of [fn]. *)

val nearest_vcs : Path.Source.t -> Vcs.t option

val files_of : Path.Source.t -> Path.Source.Set.t

(** [true] iff the path is a directory *)
val dir_exists : Path.Source.t -> bool

(** [true] iff the path is a file *)
val file_exists : Path.Source.t -> bool
