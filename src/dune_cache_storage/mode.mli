open Stdune

(** This library can take advantage of hard links to implement some functions
    more efficiently and reliably. If your file system supports hard links, we
    recommend that you use the [Hardlink] mode.

    Here is a summary of differences between the two modes:

    - In the [Copy] mode, cache entries are stored and restored by copying,
      which is both slower and takes more disk space.

    - In the [Hardlink] mode, adding a new entry to the cache is atomic, i.e. an
      existing entry is never overwritten. In the [Copy] mode, we currently do
      not guarantee atomicity: there is a small chance that an existing cache
      entry is silently overwritten, which might interfere with concurrent
      reading of that entry.

    - In the [Hardlink] mode, the cache storage must be on the same partition as
      the build tree.

    - In the [Hardlink] mode, a cache entry can be corrupted by modifying the
      hard link that points to it from the build directory. *)
type t =
  | Hardlink
  | Copy

val all : (string * t) list
val to_string : t -> string
val of_string : string -> (t, string) result
val to_dyn : t -> Dyn.t

(** Right now, this is hardcoded to be [Hardlink]. In future we plan to choose
    the mode by detecting whether hard links can be created. *)
val default : unit -> t
