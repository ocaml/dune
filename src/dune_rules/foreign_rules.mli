open Import

val foreign_flags
  :  Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> language:Foreign_language.t
  -> string list Action_builder.t

val build_o_files
  :  sctx:Super_context.t
  -> foreign_sources:Foreign.Sources.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> requires:Lib.t list Resolve.t
  -> dir_contents:Dir_contents.t
  -> Path.t Mode.Map.Multi.t Memo.t

val foreign_flags_env
  :  dir:Path.Build.t
  -> string list Action_builder.t Foreign_language.Dict.t Memo.t

(** Build the common include flags for C compilation: header hidden deps,
    library include flags, per-source include_dir_flags, and extra_deps.
    Shared between [build_o_files] and [Compile_commands]. *)
val build_include_flags
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> dir_contents:Dir_contents.t
  -> requires:Lib.t list Resolve.t
  -> src:Foreign.Source.t
  -> Command.Args.without_targets Command.Args.t

(** Construct Command.Args.t for C compilation (flags + stdlib include + include_flags).
    Does not include output (-o) or source (-c) arguments. *)
val c_compile_args
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> src:Foreign.Source.t
  -> include_flags:Command.Args.without_targets Command.Args.t
  -> Command.Args.without_targets Command.Args.t
