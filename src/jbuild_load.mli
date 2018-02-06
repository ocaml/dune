open Import
open Jbuild

module Jbuilds : sig
  type t

  val eval : t -> context:Context.t -> (Path.t * Scope.t * Stanzas.t) list Fiber.t
end

type conf =
  { file_tree : File_tree.t
  ; jbuilds   : Jbuilds.t
  ; packages  : Package.t String_map.t
  ; scopes    : Scope.t list
  }

val load
  :  ?extra_ignored_subtrees:Path.Set.t
  -> ?ignore_promoted_rules:bool
  -> unit
  -> conf
