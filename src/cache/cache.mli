open Stdune

include module type of Cache_intf

module Key : sig
  type t = Digest.t

  val of_string : string -> (t, string) Result.t

  val to_string : t -> string
end

val promotion_to_string : promotion -> string

val make_caching : (module Cache with type t = 'a) -> 'a -> (module Caching)

val cachable : Unix.file_kind -> bool

module Client = Client
module Local = Local
module Messages = Messages
