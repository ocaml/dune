open Stdune

type key = Digest.t

type metadata = Sexp.t list

module File = struct
  type t =
    { in_the_memory : Path.t
    ; in_the_build_directory : Path.Build.t
    ; digest : Digest.t
    }
end

module type memory = sig
  type t

  val promote :
       t
    -> (Path.Build.t * Digest.t) list
    -> key
    -> metadata
    -> int option
    -> (unit, string) Result.t

  val search : t -> key -> (metadata * File.t list, string) Result.t

  val set_build_dir : t -> Path.t -> t
end

module type caching = sig
  module Cache : memory

  val cache : Cache.t
end
