open! Import
open! Stdune

(** A cache that stores an association between files and string keys that will
    be deleted when dune terminates. For example this could be used to cache
    downloaded files keyed by their URLs to prevent the same file being
    downloaded multiple times in a single invocation of dune. Files are stored
    in a temporary directory. *)
type 'error t

val create : unit -> _ t

(** Retrieve the path to a file associated with a given key. If the key has no
    associated file then the function [f] is called on a path and is expected
    to create a new file at that path. The resulting file will be associated
    with the [key] in the cache. If [f] returns [Ok ()] but fails to create the
    file then a [Code_error] is raised. If [f] returns an error then the error
    will be cached and returned by subsequent calls. *)
val with_
  :  'error t
  -> key:string
  -> f:(Path.t -> (unit, 'error) result Fiber.t)
  -> (Path.t, 'error) result Fiber.t
