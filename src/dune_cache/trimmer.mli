(** Basic functionality for trimming Dune cache. *)

(* CR-someday amokhov: Reuse the testsuite and cache telemetry functionality
   from Jenga's trimmer. *)

module Trimming_result : sig
  type t =
    { trimmed_bytes : int64
    ; number_of_files_removed : int
    }
end

(** Trim the cache by removing a set of unused files so that the total freed
    space is greater or equal to the specified [goal], in bytes. A cached file
    is "unused" if there are no hard links to it from build directories.

    Unused files are removed in the order of last access, i.e. we first remove
    the least recently accessed one.

    We also remove all metadata files whose file references got broken during
    the trimming. *)
val trim : goal:int64 -> Trimming_result.t

(** Purge cache metadata files that can't be read or contain references to
    non-existing files. *)
val garbage_collect : unit -> Trimming_result.t

(** Compute the "overhead size" of the cache, that is, the total size of files
    in the cache that are not hardlinked from any build directory. *)
val overhead_size : unit -> int64
