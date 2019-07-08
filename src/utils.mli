(** Utilities that can't go in [Import] *)

open! Stdune

(** Return the absolute path to the shell and the argument to pass it
    (-c or /c). Raise in case in cannot be found. *)
val system_shell_exn : needed_to:string -> Path.t * string

(** Same as [system_shell_exn] but for bash *)
val bash_exn : needed_to:string -> Path.t

(** Return the directory where the object files for the given
    library should be stored. *)
val library_object_directory
  :  dir:Path.Build.t
  -> Lib_name.Local.t
  -> Path.Build.t

(** cmx, .a *)
val library_native_dir     : obj_dir:Path.Build.t -> Path.Build.t

(** cmo, cmi, cmt, cmti *)
val library_byte_dir       : obj_dir:Path.Build.t -> Path.Build.t
val library_public_cmi_dir : obj_dir:Path.Build.t -> Path.Build.t
val library_private_dir    : obj_dir:Path.Build.t -> Path.Build.t

(** Return the directory where the object files for the given
    executable should be stored. *)
val executable_object_directory
  :  dir:Path.Build.t
  -> string
  -> Path.Build.t

(** Raise an error about a program not found in the PATH or in the tree *)
val program_not_found
  :  ?context:string
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> _

(** Raise an error about a library not found *)
val library_not_found : ?context:string -> ?hint:string -> string -> _

val install_file
  :  package:Package.Name.t
  -> findlib_toolchain:string option
  -> string

(** Produce a line directive *)
val line_directive : filename:string -> line_number:int -> string

(** [local_bin dir] The directory which contains the local binaries viewed by
    rules defined in [dir] *)
val local_bin : Path.Build.t -> Path.Build.t
