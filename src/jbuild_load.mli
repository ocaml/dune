open Import

module Jbuild : sig
  type t =
    { path             : Path.t
    ; version          : Jbuild_types.Jbuilder_version.t
    ; sexps            : Sexp.Ast.t list
    ; visible_packages : Package.t String_map.t
    }
end

type conf =
  { file_tree : File_tree.t
  ; tree      : Alias.tree
  ; jbuilds   : Jbuild.t list
  ; packages  : Package.t String_map.t
  }

val load : unit -> conf
