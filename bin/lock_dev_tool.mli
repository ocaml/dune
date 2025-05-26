open! Import

val is_enabled : bool Lazy.t
val lock_dev_tool : Dune_pkg.Dev_tool.t -> unit Memo.t
