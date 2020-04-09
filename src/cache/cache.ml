module Log = Dune_util.Log
open Stdune
module Key = Key
include Cache_intf

let promotion_to_string = function
  | Already_promoted { in_the_cache; in_the_build_directory; _ } ->
    Printf.sprintf "%s already promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_cache)
  | Promoted { in_the_cache; in_the_build_directory; _ } ->
    Printf.sprintf "%s promoted as %s"
      (Path.Local.to_string (Path.Build.local in_the_build_directory))
      (Path.to_string in_the_cache)

let make_caching (type t) (module Caching : Cache with type t = t) (cache : t) :
    (module Caching) =
  ( module struct
    module Cache = Caching

    let cache = cache
  end )

let cachable = function
  | Unix.S_REG -> true
  | _ -> false

module Client = Client
module Local = Local
module Messages = Messages
