open! Import

val is_enabled : bool Lazy.t
val lock_ocamlformat : unit -> unit Memo.t
val lock_odoc : unit -> unit Memo.t
val lock_ocamllsp : unit -> unit Memo.t
val lock_tools : Dune_pkg.Dev_tool.t list -> unit Memo.t list
