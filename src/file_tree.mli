(** Dune representation of the source tree *)

open! Import

module Dune_file : sig
  type t =
    | Sexps        of Path.t * Sexp.Ast.t list
    | Ocaml_script of Path.t

  val path : t -> Path.t
end

module Dir : sig
  type t

  val path     : t -> Path.t
  val files    : t -> String.Set.t
  val file_paths    : t -> Path.Set.t
  val sub_dirs : t -> t String.Map.t
  val sub_dir_paths : t -> Path.Set.t
  val sub_dir_names : t -> String.Set.t

  (** Whether this directory contains raw data, as configured by a
      [.dune-fs] or [jbuild-ignore] file in one of its ancestor
      directories. *)
  val raw_data : t -> bool

  val fold
    :  t
    -> traverse_raw_data_dirs:bool
    -> init:'a
    -> f:(t -> 'a -> 'a)
    -> 'a

  (** Return the contents of the dune (or jbuild) file in this directory *)
  val dune_file : t -> Dune_file.t option
end

(** A [t] value represent a view of the source tree. It is lazily
    constructed by scanning the file system and interpreting [.dune-fs]
    files, as well as [jbuild-ignore] files for backward
    compatibility. *)
type t

val load : ?extra_ignored_subtrees:Path.Set.t -> Path.t -> t

(** Passing [~traverse_raw_data_dirs:true] to this functions causes the
    whole source tree to be deeply scanned, including raw_data
    directories. *)
val fold
  :  t
  -> traverse_raw_data_dirs:bool
  -> init:'a
  -> f:(Dir.t -> 'a -> 'a)
  -> 'a

val root : t -> Dir.t

val find_dir : t -> Path.t -> Dir.t option

val files_of : t -> Path.t -> Path.Set.t

(** [true] iff the path is either a directory or a file *)
val exists : t -> Path.t -> bool

(** [true] iff the path is a file *)
val file_exists : t -> Path.t -> string -> bool

val files_recursively_in : t -> ?prefix_with:Path.t -> Path.t -> Path.Set.t
