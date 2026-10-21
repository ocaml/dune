(** Select an in-place formatter for a generated correction. *)

open Import

val format_config : dir:Path.Build.t -> Format_config.t Memo.t
val ocamlformat_flag : Ml_kind.t -> string
val ocamlformat_dev_tool_lock_dir_exists : unit -> bool Memo.t

(** Dependencies that affect OCamlFormat's result. *)
val ocamlformat_config_deps : dir:Path.Build.t -> unit Action_builder.t

(** Format a generated correction in place when formatting is enabled for its
    source, then run [diff]. The generated path is deliberately not added as a
    dependency. *)
val format_diff
  :  Super_context.t
  -> dir:Path.Build.t
  -> source:Path.t
  -> target:Path.Build.t
  -> diff:Action.t
  -> Action.Full.t Action_builder.t
