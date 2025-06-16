open Import

(** [t] represents the source file associated to a cram test. *)
type t =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

val to_dyn : t -> Dyn.t

(** Checks if a filename has the ".t" suffix for a cram test. *)
val is_cram_suffix : Filename.t -> bool

(** The "run.t" filename for directory cram tests. *)
val fname_in_dir_test : Filename.t

(** The [name] of a cram test. If this is a file test, then it will be the file
    name without the cram suffix. If this is a directory test, then it will be
    the directory name without the cram suffix. *)
val name : t -> string

(** The [path] associated to a cram test. If this is a file test, then it will
    be the file. If this is a directory test, then it will be the directory. *)
val path : t -> Path.Source.t

(** The [script] of a cram test. If this is a file test, then it will be the
    file. If this is a directory test, then it will be the "run.t" file inside
    that directory. *)
val script : t -> Path.Source.t
