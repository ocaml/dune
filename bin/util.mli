open Import

val find_in_path_exn : string -> Path.t
val resolve_prog : string -> string
val resolve_program_path : string -> Path.t

type checked =
  | In_build_dir of (Context.t * Path.Source.t)
  | In_private_context of Path.Build.t
  | In_install_dir of (Context.t * Path.Source.t)
  | In_source_dir of Path.Source.t
  | External of Path.External.t

val check_path : Context.t list -> Path.t -> checked
val restore_cwd_and_execve : Workspace_root.t -> string -> string list -> Env.t -> 'a
val setup : unit -> Dune_rules.Main.build_system Memo.t
