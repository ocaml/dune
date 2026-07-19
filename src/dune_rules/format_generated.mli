open Import

val format_config : dir:Path.Build.t -> Format_config.t Memo.t

val dialect_formatter
  :  Format_config.t
  -> dialects:Dialect.DB.t
  -> Path.t
  -> (Ml_kind.t * Dialect.Format.t) option

val ocamlformat_flag : Ml_kind.t -> string
val ocamlformat_dev_tool_lock_dir_exists : unit -> bool Memo.t

(** Dependencies that affect OCamlFormat's result. *)
val ocamlformat_config_deps : dir:Path.Build.t -> unit Action_builder.t
