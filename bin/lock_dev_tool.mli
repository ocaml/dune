open! Import

val is_enabled : bool Lazy.t
val lock_dev_tool : Context_name.t -> Dune_pkg.Dev_tool.t -> unit Memo.t
