(** Dune representation of the source tree *)
open! Stdune

open! Import

module Dune_file : sig
  val fname : string

  val jbuild_fname : string

  type kind = private
    | Plain
    | Ocaml_script

  type t

  (** We release the memory taken by s-exps as soon as it is used, unless
      [kind = Ocaml_script]. In which case that optimization is incorrect as we
      need to re-parse in every context. *)
  val get_static_sexp_and_possibly_destroy : t -> Dune_lang.Ast.t list

  val kind : t -> kind

  val path : t -> Path.Source.t
end

module Dir : sig
  type t

  type error = Missing_run_t of Cram_test.t

  val cram_tests : t -> (Cram_test.t, error) result list

  val path : t -> Path.Source.t

  val files : t -> String.Set.t

  val file_paths : t -> Path.Source.Set.t

  val fold_sub_dirs : t -> init:'a -> f:(basename:string -> t -> 'a -> 'a) -> 'a

  val fold_dune_files :
       t
    -> init:'acc
    -> f:(basename:string option -> t -> Dune_file.t -> 'acc -> 'acc)
    -> 'acc

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

val find_dir_specified_on_command_line : dir:Path.Source.t -> Dir.t
