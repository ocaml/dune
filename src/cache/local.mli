open Stdune
open Cache_intf

include Cache

val default_root : unit -> Path.t

module Metadata_file : sig
  type t =
    { metadata : Sexp.t list
    ; files : File.t list
    }

  val to_sexp : t -> Sexp.t

  val of_sexp : Sexp.t -> (t, string) result

  val to_string : t -> string

  val of_string : string -> (t, string) result

  val parse : Path.t -> (t, string) result
end

val promote_sync :
     t
  -> (Path.Build.t * Digest.t) list
  -> Key.t
  -> metadata
  -> repository:int option
  -> duplication:Duplication_mode.t option
  -> (Metadata_file.t * promotion list, string) Result.t

val make :
     ?root:Path.t
  -> ?duplication_mode:Duplication_mode.t
  -> ?log:(User_message.Style.t Pp.t list -> unit)
  -> ?warn:(User_message.Style.t Pp.t list -> unit)
  -> handler
  -> (t, string) Result.t

val duplication_mode : t -> Duplication_mode.t

(** The size overhead of cached files. That is, the total size of cached files
    that are not linked in a build directory. *)
val size : t -> int

module Trimming_result : sig
  type t =
    { trimmed_files_size : int
    ; trimmed_files : Path.t list
    ; trimmed_metafiles : Path.t list
    }
end

(** [trim cache size] removes files from [cache], starting with the least
    recently used one, until [size] bytes have been freed. *)
val trim : t -> int -> Trimming_result.t

(** Purge invalid or incomplete cached rules. *)
val garbage_collect : t -> Trimming_result.t

(** Path to a metadata file *)
val path_metadata : t -> Key.t -> Path.t

(** Path to a data file *)
val path_data : t -> Key.t -> Path.t
