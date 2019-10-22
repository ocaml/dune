type key = Stdune.Digest.t

type metadata = Stdune.Sexp.t list

module File = struct
  type t =
    { in_the_memory : Stdune.Path.t
    ; in_the_build_directory : Stdune.Path.Build.t
    ; digest : Stdune.Digest.t
    }
end

module type memory = sig
  type t

  val promote :
       t
    -> (Stdune.Path.Build.t * Stdune.Digest.t) list
    -> key
    -> metadata
    -> int option
    -> (unit, string) Result.t

  val search : t -> key -> (metadata * File.t list, string) Result.t

  val set_build_dir : t -> Stdune.Path.t -> t
end

module type caching = sig
  module Cache : memory

  val cache : Cache.t
end
