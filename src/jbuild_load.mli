open Import

module Jbuilds : sig
  type t

  val eval : t -> context:Context.t -> (Path.t * Jbuild_types.Stanzas.t) list Future.t
end

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; jbuilds   : Jbuilds.t
  ; packages  : Package.t String_map.t
  }

val load : unit -> conf
