open Import

val is_enabled : bool Lazy.t
val lock_dev_tool : Dune_pkg.Dev_tool.t -> Dune_pkg.Network_cap.t -> unit Memo.t
