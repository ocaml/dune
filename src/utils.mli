(** Utilities that can't go in [Import] *)

open! Import

(** Return the absolute path to the shell and the argument to pass it (-c or /c). Raise in
    case in cannot be found. *)
val system_shell_exn : needed_to:string -> Path.t * string

(** Same as [system_shell_exn] but for bash *)
val bash_exn : needed_to:string -> Path.t

(** Convert a signal number to a name: INT, TERM, ... *)
val signal_name : int -> string

(** [jbuild_file_in ~dir = Path.relative dir "jbuild"]. *)
val jbuild_file_in : dir:Path.t -> Path.t

(** Nice description of a target *)
val describe_target : Path.t -> string

(** Return the directory where the object files for the given
    library should be stored. *)
val library_object_directory
  :  dir:Path.t
  -> string
  -> Path.t

(** Return the directory where the object files for the given
    executable should be stored. *)
val executable_object_directory
  :  dir:Path.t
  -> string
  -> Path.t

type target_kind =
  | Regular of string (* build context *) * Path.t
  | Alias   of string (* build context *) * Path.t
  | Other of Path.t

(** Return the name of an alias from its stamp file *)
val analyse_target : Path.t -> target_kind

(** Raise an error about a program not found in the PATH or in the tree *)
val program_not_found
  :  ?context:string
  -> ?hint:string
  -> string
  -> _

(** Raise an error about a library not found *)
val library_not_found : ?context:string -> ?hint:string -> string -> _

(** [\["-g"\]] if [!Clflags.g] and [\[\]] otherwise *)
val g : unit -> string list

val install_file : package:string -> findlib_toolchain:string option -> string

(** Digest files with caching *)
module Cached_digest : sig
  (** Digest the contents of the following file *)
  val file : Path.t -> Digest.t

  (** Clear the following digest from the cache *)
  val remove : Path.t -> unit

  (** Dump/load the cache to/from the disk *)
  val dump : unit -> unit
  val load : unit -> unit
end
