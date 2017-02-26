open Import

module Jbuild : sig
  type t

  val eval : t -> context:Context.t -> Path.t * Jbuild_types.Stanzas.t
end

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; jbuilds   : Jbuild.t list
  ; packages  : Package.t String_map.t
  }

val load : unit -> conf
