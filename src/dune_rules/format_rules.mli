open Import

(** Setup automatic format rules for the given dir. If tools like ocamlformat
    are not available in $PATH, just display an error message when the alias is
    built.

    This must be called from inside the [formatted_dir_basename] sub-directory. *)
val gen_rules : Super_context.t -> output_dir:Path.Build.t -> unit Memo.t

(** This must be called from the main directory, i.e. the ones containing the
    source files and the the [formatted_dir_basename] sub-directory. *)
val setup_alias : dir:Path.Build.t -> unit Memo.t

val formatted_dir_basename : Filename.t

val format_action
  :  expander:Expander.t
  -> dialects:Dialect.DB.t
  -> config:Format_config.t
  -> dir:Path.Build.t
  -> ext:string
  -> input:Path.Build.t
  -> output:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t option

val with_config : dir:Path.Build.t -> (Format_config.t -> unit Memo.t) -> unit Memo.t
