open Stdune
open Cache_intf

include Cache

(** The default root directory of the local cache. *)
val default_root : unit -> Path.t

(** A matadata file contains a list of [files] produced by a cached build rule,
    along with some [metadata] that can be empty.

    One example of what can be included in the [metadata] field is a git commit
    at which the [files] were built, which makes it possible to exchange cache
    entries relevant to a specific commit between local and distributed caches. *)
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

(** Like [promote] but also returns the resulting metadata and promotions. *)
val promote_sync :
     t
  -> (Path.Build.t * Digest.t) list
  -> Key.t
  -> metadata
  -> repository:int option
  -> duplication:Duplication_mode.t option
  -> (Metadata_file.t * promotion list, string) Result.t

(** Create a local cache. The only required argument is a handler for commands
    from the cache, such as [Dedup] that tell Dune that some files can be
    replaced with hardlinks to their cached versions. The [root] argument
    defaults to the [default_root]. If [duplication_mode] is omitted, we attempt
    to detect whether hardlinks are supported and use [Hardlink] if they are,
    falling back to [Copy] otherwise. *)
val make :
     ?root:Path.t
  -> ?duplication_mode:Duplication_mode.t
  -> ?log:(User_message.Style.t Pp.t list -> unit)
  -> ?warn:(User_message.Style.t Pp.t list -> unit)
  -> command_handler:(command -> unit)
  -> unit
  -> (t, string) Result.t

(** The deduplication mode that was set or detected automatically (if omitted)
    during the local cache creation with the function [make]. *)
val duplication_mode : t -> Duplication_mode.t

(** The overhead size of the cache, that is, the total size of files in the
    cache that are not linked from any build directory. *)
val overhead_size : t -> int

module Trimming_result : sig
  type t =
    { trimmed_files_size : int
    ; trimmed_files : Path.t list
    ; trimmed_metafiles : Path.t list
    }
end

(** Return a list of unexpected paths that exist in the root directory of the
    cache. A non-empty result suggests that the cache directory contains
    multiple versions of the cache, making [trim] and [garbage_collect] less
    effective. *)
val detect_unexpected_dirs_under_cache_root :
  t -> (Path.t list, Unix.error) result

(** [trim cache size] removes files from [cache], starting with the least
    recently used one, until [size] bytes have been freed. *)
val trim : t -> int -> Trimming_result.t

(** Purge invalid or incomplete cached rules. *)
val garbage_collect : t -> Trimming_result.t
