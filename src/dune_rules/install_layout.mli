(** Install layout machinery for [(deps (package ...))].

    Scoped install layout materialised under
    [_build/install/<context>/.packages/<digest>/] containing only the declared
    package dependencies. The consumer side (queries, env construction) lives
    here, low in the dependency stack; the producer side ([gen_rules] for the
    directory and the data resolver callback) lives in [install_rules], which
    is high in the stack and has access to install entries. The two are
    connected by an [Fdecl] seam: [install_rules] calls [set_entry_resolver]
    at module init. *)

open Import

type entry =
  { src : Path.t
  ; relative : Path.Source.t
  }

(** Must be called during initialization to wire up install entry resolution.
    Called from [install_rules]. *)
val set_entry_resolver : (Context_name.t -> Package.Name.t -> entry list Memo.t) -> unit

(** Files that would be in the layout for a set of packages. Consumers use
    this to set up file-level dependencies. *)
val files : Context_name.t -> Package.Name.t list -> Path.t list Memo.t

(** The [lib] subdirectory of the layout for a set of packages. Suitable for
    adding to [OCAMLPATH]. *)
val lib_root : Context_name.t -> Package.Name.t list -> Path.Build.t

module Key : sig
  type encoded = Digest.t

  module Decoded : sig
    type t = private { packages : Package.Name.t list }
  end

  val decode : encoded -> Decoded.t
end

(** The memoized entries computation. Exposed for [install_rules]'s
    [gen_rules] which materializes the layout into symlinks. *)
val entries_memo
  : (Context_name.t * Digest.t, (Path.t * Path.Build.t * Path.Source.t) list) Memo.Table.t
