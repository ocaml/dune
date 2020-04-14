(** Utilities that can't go in [Import] *)

open! Stdune

val system_shell_exn : needed_to:string -> Path.t * string
(** Return the absolute path to the shell and the argument to pass it (-c or
    /c). Raise in case in cannot be found. *)

val bash_exn : needed_to:string -> Path.t
(** Same as [system_shell_exn] but for bash *)

val program_not_found :
  ?context:Context_name.t -> ?hint:string -> loc:Loc.t option -> string -> _
(** Raise an error about a program not found in the PATH or in the tree *)

val library_not_found : ?context:Context_name.t -> ?hint:string -> string -> _
(** Raise an error about a library not found *)

val install_file :
  package:Package.Name.t -> findlib_toolchain:Context_name.t option -> string

val line_directive : filename:string -> line_number:int -> string
(** Produce a line directive *)

val local_bin : Path.Build.t -> Path.Build.t
(** [local_bin dir] The directory which contains the local binaries viewed by
    rules defined in [dir] *)
