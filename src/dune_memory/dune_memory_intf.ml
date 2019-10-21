open Stdune

type key = Digest.t

type metadata = Sexp.t list

type promotion =
  | Already_promoted of Path.Build.t * Path.t * Digest.t
  | Promoted of Path.Build.t * Path.t * Digest.t

module File = struct
  type t =
    { in_the_memory : Path.t
    ; in_the_build_directory : Path.Build.t
    ; digest : Digest.t
    }
end

type repository =
  { directory : string
  ; remote : string
  ; commit : string
  }

type command = Dedup of (Path.Build.t * Path.t * Digest.t)

type handler = command -> unit

module type memory = sig
  type t

  val with_repositories : t -> repository list -> t

  val promote :
       t
    -> (Path.Build.t * Digest.t) list
    -> key
    -> metadata
    -> int option
    -> (unit, string) Result.t

  val search : t -> key -> (metadata * File.t list, string) Result.t

  val set_build_dir : t -> Path.t -> t

  val teardown : t -> unit
end
