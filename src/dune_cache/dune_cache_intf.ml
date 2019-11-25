open Stdune

type metadata = Sexp.t list

module File = struct
  type t =
    { in_the_cache : Path.t
    ; in_the_build_directory : Path.Build.t
    ; digest : Digest.t
    }
end

type promotion =
  | Already_promoted of File.t
  | Promoted of File.t

type repository =
  { directory : string
  ; remote : string
  ; commit : string
  }

type trimming_result =
  { trimmed_files_size : int
  ; trimmed_files : Path.t list
  ; trimmed_metafiles : Path.t list
  }

type command = Dedup of File.t

type handler = command -> unit

module Duplication_mode = struct
  type t =
    | Copy
    | Hardlink

  let all = [ ("copy", Copy); ("hardlink", Hardlink) ]

  let of_string repr =
    match List.assoc all repr with
    | Some mode -> Result.Ok mode
    | None -> Result.Error (Format.sprintf "invalid duplication mode: %s" repr)

  let to_string = function
    | Copy -> "copy"
    | Hardlink -> "hardlink"
end

module type Cache = sig
  type t

  val with_repositories : t -> repository list -> t

  val promote :
       t
    -> (Path.Build.t * Digest.t) list
    -> Key.t
    -> metadata
    -> repository:int option
    -> duplication:Duplication_mode.t option
    -> (unit, string) Result.t

  val search : t -> Key.t -> (metadata * File.t list, string) Result.t

  val retrieve : t -> File.t -> Path.t

  val deduplicate : t -> File.t -> unit

  val set_build_dir : t -> Path.t -> t

  val teardown : t -> unit
end

module type Caching = sig
  module Cache : Cache

  val cache : Cache.t
end

type caching = (module Caching)
