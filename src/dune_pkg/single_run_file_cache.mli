open! Import
open! Stdune

(** A cache that stores an association between files and string keys that will
    be deleted when dune terminates. For example this could be used to cache
    downloaded files keyed by their URLs to prevent the same file being
    downloaded multiple times in a single invocation of dune. Files are stored
    in a temporary directory. *)
type t

val create : unit -> t

(** Retrieve the path to a file associated with a given key. If the key has no
    associated file then the function [f] is called on a path and is expected
    to create a new file at that path. The resulting file will be associated
    with the [key] in the cache. If [f] returns [Ok ()] but fails to create the
    file then a [Code_error] is raised. *)
val with_
  :  t
  -> key:string
  -> f:(Path.t -> (unit, 'a) result Fiber.t)
  -> (Path.t, 'a) result Fiber.t
